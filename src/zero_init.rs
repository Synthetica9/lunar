/// Marker trait to say "this is safe to zero initialize"
///
/// # Safety
/// type must be safe to be initialized as zero.
pub unsafe trait ZeroInit
where
    Self: Sized,
{
    fn zero_box() -> Box<Self> {
        unsafe {
            // Safety: guaranteed by caller due to unsafe trait.
            Box::new_zeroed().assume_init()
        }
    }
}
