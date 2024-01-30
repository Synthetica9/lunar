/// RAII guard to implement a poor man's defer
///
/// Happy path:
///
/// ```
/// use lunar::poison_drop::PoisonDrop;
/// {
///     let mut drop = PoisonDrop::new();
///     drop.safe = true;
/// }
/// ```
///
/// Unhappy path:
///
/// ```should_panic
/// use lunar::poison_drop::PoisonDrop;
/// {
///     let mut drop = PoisonDrop::new();
///     // Never marked safe!
/// }
/// ```
pub struct PoisonDrop {
    pub safe: bool,
}

impl PoisonDrop {
    pub fn new() -> Self {
        Self { safe: false }
    }
}

impl Drop for PoisonDrop {
    fn drop(&mut self) {
        if !self.safe {
            panic!("Poison drop triggered!");
        }
    }
}
