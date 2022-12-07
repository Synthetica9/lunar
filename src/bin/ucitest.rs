use lunar::uci::UCIState;

pub fn main() {
    let mut state = UCIState::new();

    let uci_session = &[
        "uci",
        "isready",
        "ucinewgame",
        "position fen 8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1",
        "d",
        "go depth 10",
        "wait",
        "quit",
    ];

    for command in uci_session {
        state.interpret(command).unwrap();
    }
}
