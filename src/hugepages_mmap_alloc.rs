// https://stackoverflow.com/questions/69542979/how-can-i-align-memory-on-the-heap-in-a-box-in-a-convenient-way

use std::alloc::*;
use std::fs::File;
use std::num::NonZeroUsize;
use std::ptr::{slice_from_raw_parts_mut, NonNull};

use nix::sys::mman::{mmap, munmap, MapFlags, ProtFlags};

pub struct HugePagesAlloc;

unsafe impl Allocator for HugePagesAlloc {
    fn allocate(&self, layout: Layout) -> Result<NonNull<[u8]>, AllocError> {
        let length = NonZeroUsize::new(layout.size()).ok_or(AllocError)?;

        let prot_flags = ProtFlags::PROT_READ | ProtFlags::PROT_WRITE;
        let map_flags = MapFlags::MAP_PRIVATE | MapFlags::MAP_ANONYMOUS | MapFlags::MAP_HUGETLB;
        let f: Option<File> = None;
        let mmap_result =
            unsafe { mmap(None, length, prot_flags, map_flags, f, 0) }.map_err(|err| {
                eprintln!("mmap: {err:?}");
                AllocError
            })?;
        let slc = slice_from_raw_parts_mut(mmap_result.cast::<u8>(), length.get());
        let nonnull = NonNull::new(slc).ok_or(AllocError)?;
        Ok(nonnull)
    }
    unsafe fn deallocate(&self, ptr: NonNull<u8>, layout: Layout) {
        munmap(ptr.as_ptr().cast(), layout.size()).expect("munmap failed");
    }
}
