use crate::PageRef;

#[derive(Debug, Clone)]
pub enum EmptyTableState {
    Start,
    ReadPage { page: PageRef },
}
