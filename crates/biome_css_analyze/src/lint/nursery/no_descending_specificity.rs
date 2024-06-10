use std::{
    collections::BTreeMap,
    ops::{Add, AddAssign, Sub},
};

use biome_analyze::{
    context::RuleContext, declare_rule, Ast, Phases, QueryMatch, Queryable, Rule, RuleDiagnostic,
    RuleSource, Visitor, VisitorContext,
};
use biome_console::markup;
use biome_css_syntax::{
    AnyCssPseudoClass, AnyCssSelector, AnyCssSimpleSelector, AnyCssSubSelector, CssComplexSelector,
    CssCompoundSelector, CssLanguage, CssSelectorList, CssSyntaxKind,
};
use biome_rowan::{AstNode, SyntaxNode, SyntaxNodeOptionExt, TextRange, WalkEvent};

declare_rule! {
    /// Disallow selectors of lower specificity from coming after overriding selectors of higher specificity.
    ///
    /// Put context and details about the rule.
    /// As a starting point, you can take the description of the corresponding _ESLint_ rule (if any).
    ///
    /// Try to stay consistent with the descriptions of implemented rules.
    ///
    /// Add a link to the corresponding stylelint rule (if any):
    ///
    /// ## Examples
    ///
    /// ### Invalid
    ///
    /// ```css,expect_diagnostic
    /// p {}
    /// ```
    ///
    /// ### Valid
    ///
    /// ```css
    /// p {
    ///   color: red;
    /// }
    /// ```
    ///
    pub NoDescendingSpecificity {
        version: "next",
        name: "noDescendingSpecificity",
        language: "css",
        recommended: true,
        sources: &[RuleSource::Stylelint("no-descending-specificity")],
    }
}

/// Selector weight of specificity
/// See https://developer.mozilla.org/en-US/docs/Web/CSS/Specificity
#[derive(Default, Debug, PartialEq, PartialOrd, Clone)]
struct CssSpecificity {
    id_col: i32,
    class_col: i32,
    type_col: i32,
}

impl CssSpecificity {
    fn new(id_col: i32, class_col: i32, type_col: i32) -> Self {
        CssSpecificity {
            id_col,
            class_col,
            type_col,
        }
    }

    fn increment_id(&self) -> Self {
        CssSpecificity {
            id_col: self.id_col + 1,
            ..self.clone()
        }
    }

    fn increment_class(&self) -> Self {
        CssSpecificity {
            class_col: self.class_col + 1,
            ..self.clone()
        }
    }

    fn increment_type(&self) -> Self {
        CssSpecificity {
            type_col: self.type_col + 1,
            ..self.clone()
        }
    }
}

impl Add for CssSpecificity {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        CssSpecificity {
            id_col: self.id_col + rhs.id_col,
            class_col: self.class_col + rhs.class_col,
            type_col: self.type_col + rhs.type_col,
        }
    }
}
impl Sub for CssSpecificity {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        CssSpecificity {
            id_col: self.id_col - rhs.id_col,
            class_col: self.class_col - rhs.class_col,
            type_col: self.type_col - rhs.type_col,
        }
    }
}

#[derive(Default)]
struct CssSpecificityVisitor {
    found_specificity: BTreeMap<String, (CssSpecificity, CssCompoundSelector)>,
}

fn calc_pseudo_class_specificity(node: &AnyCssPseudoClass) -> CssSpecificity {
    dbg!(&node);
    let mut specificity = CssSpecificity::default();
    match node {
        // 0-1-0
        AnyCssPseudoClass::CssPseudoClassFunctionNth(_)
        | AnyCssPseudoClass::CssPseudoClassIdentifier(_) => {
            specificity = specificity.increment_class();
        }        
        
        // like `:is(.class1,.class2)`
        AnyCssPseudoClass::CssPseudoClassFunctionIdentifier(_) => todo!(),
        AnyCssPseudoClass::CssPseudoClassFunctionCompoundSelector(_) => todo!(),
        AnyCssPseudoClass::CssPseudoClassFunctionSelector(_) => todo!(),
        AnyCssPseudoClass::CssPseudoClassFunctionRelativeSelectorList(_) => todo!(),
        AnyCssPseudoClass::CssPseudoClassFunctionCompoundSelectorList(selector_list) => {
            
        },
        AnyCssPseudoClass::CssPseudoClassFunctionSelectorList(selector_list) => {
            for selector in selector_list.selectors() {
                if let Ok(selector) = selector {
                    specificity = specificity + calc_any_selector_specificity(&selector);
                }
            }
        }
        AnyCssPseudoClass::CssPseudoClassFunctionValueList(_) => todo!(),

        // 0-0-0
        AnyCssPseudoClass::CssBogusPseudoClass(_) => todo!(),
    }
    specificity
}

fn calc_compound_specificity(node: &CssCompoundSelector) -> CssSpecificity {
    let mut specificity = CssSpecificity::default();
    if let Some(selector) = node.simple_selector() {
        match selector {
            AnyCssSimpleSelector::CssTypeSelector(selector) => {
                specificity = specificity.increment_type();
            }
            AnyCssSimpleSelector::CssUniversalSelector(_) => {
                // universal selector weight is 0-0-0, so do nothing
                // See https://developer.mozilla.org/en-US/docs/Web/CSS/Specificity#no_value
            }
        }
    }

    let selector_list = node.sub_selectors();
    for selector in selector_list {
        match selector {
            // weight 1-0-0
            AnyCssSubSelector::CssIdSelector(_) => {
                specificity = specificity.increment_id();
            }
            // weight 0-1-0
            AnyCssSubSelector::CssClassSelector(_) | AnyCssSubSelector::CssAttributeSelector(_) => {
                specificity = specificity.increment_class();
            }
            // pseudo class weight 0-1-0
            AnyCssSubSelector::CssPseudoClassSelector(pseudo) => {
                if let Ok(pseudo) = pseudo.class() {
                    specificity = specificity + calc_pseudo_class_specificity(&pseudo);
                }
            }
            // weight 0-0-1
            AnyCssSubSelector::CssPseudoElementSelector(_) => {
                specificity = specificity.increment_type();
            }
            // unknown
            AnyCssSubSelector::CssBogusSubSelector(_) => {}
        }
    }
    specificity
}

fn calc_complex_specificity(node: &CssComplexSelector) -> CssSpecificity {
    let left_specificity = node
        .left()
        .and_then(|left| Ok(calc_any_selector_specificity(&left)))
        .unwrap_or_default();
    let right_specificity = node
        .right()
        .and_then(|right| Ok(calc_any_selector_specificity(&right)))
        .unwrap_or_default();

    return left_specificity + right_specificity;
}

fn calc_any_selector_specificity(node: &AnyCssSelector) -> CssSpecificity {
    match node {
        AnyCssSelector::CssBogusSelector(_) => CssSpecificity::default(),
        AnyCssSelector::CssComplexSelector(complex) => calc_complex_specificity(complex),
        AnyCssSelector::CssCompoundSelector(compound) => calc_compound_specificity(compound),
    }
}

impl CssSpecificityVisitor {
    fn on_enter(&mut self, node: &SyntaxNode<CssLanguage>) {
        if CssSyntaxKind::CSS_ROOT == node.kind() {
            dbg!(&node);
        }
        if Some(CssSyntaxKind::CSS_QUALIFIED_RULE) != node.grand_parent().kind() {
            // dbg!(&node);
            return;
        }else{
            dbg!(&node);
        }

        if let Some(node) = CssComplexSelector::cast_ref(node) {
            let specificity = calc_complex_specificity(&node);
            dbg!(node.text(), specificity);
        } else if let Some(node) = CssCompoundSelector::cast_ref(node) {
            let specificity = calc_compound_specificity(&node);
            dbg!(node.text(), specificity);
        }
    }
    fn on_leave(&mut self, node: &SyntaxNode<CssLanguage>, ctx: VisitorContext<CssLanguage>) {}
}

impl Visitor for CssSpecificityVisitor {
    type Language = CssLanguage;

    fn visit(
        &mut self,
        event: &WalkEvent<SyntaxNode<Self::Language>>,
        ctx: VisitorContext<Self::Language>,
    ) {
        match event {
            WalkEvent::Enter(node) => self.on_enter(node),
            WalkEvent::Leave(node) => self.on_leave(node, ctx),
        }
    }
}

#[derive(Clone)]
pub struct AscendingSelector {
    lower: CssCompoundSelector,
}

impl QueryMatch for AscendingSelector {
    fn text_range(&self) -> TextRange {
        self.lower.range()
    }
}

impl Queryable for AscendingSelector {
    type Input = Self;

    type Output = Self;

    type Language = CssLanguage;

    type Services = ();

    fn build_visitor(
        analyzer: &mut impl biome_analyze::AddVisitor<Self::Language>,
        root: &<Self::Language as biome_rowan::Language>::Root,
    ) {
        analyzer.add_visitor(Phases::Syntax, CssSpecificityVisitor::default)
    }

    fn unwrap_match(services: &biome_analyze::ServiceBag, query: &Self::Input) -> Self::Output {
        query.clone()
    }
}

impl Rule for NoDescendingSpecificity {
    type Query = AscendingSelector;
    type State = ();
    type Signals = Option<Self::State>;
    type Options = ();

    fn run(ctx: &RuleContext<Self>) -> Option<Self::State> {
        let query = ctx.query();
        dbg!(&query.lower);
        None
    }

    fn diagnostic(_: &RuleContext<Self>, node: &Self::State) -> Option<RuleDiagnostic> {
        //
        // Read our guidelines to write great diagnostics:
        // https://docs.rs/biome_analyze/latest/biome_analyze/#what-a-rule-should-say-to-the-user
        //
        return None;
    }
}
