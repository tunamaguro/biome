use crate::formatter::TrailingSeparator;
use crate::prelude::*;
use rome_js_syntax::{TsEnumDeclaration, TsEnumDeclarationFields};

impl FormatNode for TsEnumDeclaration {
    fn format_fields(&self, formatter: &Formatter) -> FormatResult<FormatElement> {
        let TsEnumDeclarationFields {
            const_token,
            enum_token,
            id,
            members,
            l_curly_token,
            r_curly_token,
        } = self.as_fields();

        let const_token = const_token
            .with_or_empty(|const_token| formatted![formatter, const_token, space_token()]);
        let enum_token =
            enum_token.with(|enum_token| formatted![formatter, enum_token, space_token()]);
        let id = id.with(|id| formatted![formatter, id, space_token()]);

        let members =
            formatter.format_separated(&members, || token(","), TrailingSeparator::default())?;
        let list = formatter.format_delimited_soft_block_spaces(
            &l_curly_token?,
            join_elements(soft_line_break_or_space(), members),
            &r_curly_token?,
        )?;

        formatted![formatter, const_token, enum_token, id, list]
    }
}
