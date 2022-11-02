struct TranspositionTable {
    table: Vec<Option<TranspositionEntry>>,
    num_entries: usize,
    // TODO: keep track of occupancy
}

enum ScoreType {
    Exact,
    UpperBound,
    LowerBound,
}

// TODO: drop the option, use zero hash if not present.
struct TranspositionEntry {
    hash: ZobristHash,
    depth: u8,
    score: Millipawns,
    score_type: ScoreType,
    best_move: Option<Move>,
}

const ENTRY_SIZE: usize = mem::size_of::<Option<TranspositionEntry>>();

impl TranspositionTable {
    fn new(bytes: usize) -> TranspositionTable {
        let mut tbl = TranspositionTable {
            table: Vec::with_capacity(TranspositionTable::num_entries(bytes)),
        };
        tbl.resize(bytes);
        tbl
    }

    fn resize(&mut self, bytes: usize) {
        todo!();
    }

    pub const fn num_entries(bytes: usize) -> usize {
        bytes / ENTRY_SIZE
    }

    pub const fn get_num_entries(&self) -> usize {
        self.table.len()
    }

    pub fn get(&self, hash: ZobristHash) -> Option<TranspositionEntry> {
        todo!();
    }

    pub fn put(&mut self, entry: TranspositionEntry) {
        todo!();
    }
}

impl TranspositionEntry {
    fn should_replace(&self, other: &TranspositionEntry) -> bool {
        // TODO: implement. Currently use always_replace
        return true;
    }
}
