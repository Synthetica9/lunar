use lunar::uci::UCIState;

pub fn main() {
    let mut state = UCIState::new();

    let uci_session = &[
        "uci",
        "isready",
        "ucinewgame",
        "position startpos",
        "d",
        "go depth 6",
        "wait",
        "quit",
    ];

    for command in uci_session {
        state.interpret(command).unwrap();
    }
}
