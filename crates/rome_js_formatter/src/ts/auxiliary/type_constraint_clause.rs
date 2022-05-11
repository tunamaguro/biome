use crate::prelude::*;
use rome_js_syntax::TsTypeConstraintClause;

impl FormatNode for TsTypeConstraintClause {
    fn format_fields(&self, formatter: &Formatter) -> FormatResult<FormatElement> {
        let extends = self.extends_token().format(formatter)?;
        let ty = self.ty().format(formatter)?;
        formatted![formatter, extends, space_token(), ty]
    }
}
