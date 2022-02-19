use crate::*;
use chilic_error::{DiagnosticResult, SyntaxError};
use chilic_ir::{
    entity::{EntityKind, Visibility},
    item::{Item, ItemKind},
};
use chilic_span::Span;

impl Parser {
    pub(crate) fn parse_item(&mut self) -> DiagnosticResult<Vec<Item>> {
        self.skip_redundant_tokens();

        let visibility = if mat!(self, Pub) {
            Visibility::Public
        } else {
            Visibility::Private
        };

        if mat!(self, Use) {
            let uses = self.parse_use(visibility)?;

            let items: Vec<Item> = uses
                .into_iter()
                .map(|use_| {
                    let span = use_.span.clone();
                    Item::new(self.module_info, ItemKind::UseDecl(use_), span)
                })
                .collect();

            Ok(items)
        } else if mat!(self, Type) {
            let items = self
                .parse_decl_item(EntityKind::Type, visibility)
                .map(|item| vec![item])?;

            Ok(items)
        } else if mat!(self, Let) {
            let items = self
                .parse_decl_item(EntityKind::Value, visibility)
                .map(|item| vec![item])?;

            Ok(items)
        } else if mat!(self, Foreign) {
            let start_span = self.previous().span.clone();
            let entitys = self.parse_foreign_block()?;
            let items = entitys
                .iter()
                .map(|entity| {
                    Item::new(
                        self.module_info,
                        ItemKind::Entity(entity.clone()),
                        Span::merge(&start_span, self.previous_span_ref()),
                    )
                })
                .collect();

            Ok(items)
        } else {
            Err(SyntaxError::expected(
                self.span_ref(),
                &format!("an item, got `{}`", self.peek().lexeme),
            ))
        }
    }

    pub(crate) fn parse_decl_item(
        &mut self,
        kind: EntityKind,
        visibility: Visibility,
    ) -> DiagnosticResult<Item> {
        let start_span = self.previous().span.clone();

        if kind == EntityKind::Value && mat!(self, Foreign) {
            let entity = self.parse_foreign_single(visibility)?;

            Ok(Item::new(
                self.module_info,
                ItemKind::Entity(entity),
                Span::merge(&start_span, self.previous_span_ref()),
            ))
        } else {
            let entity = self.parse_entity(kind, visibility, true)?;

            Ok(Item::new(
                self.module_info,
                ItemKind::Entity(entity),
                Span::merge(&start_span, self.previous_span_ref()),
            ))
        }
    }
}
