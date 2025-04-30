// https://stackoverflow.com/questions/69542979/how-can-i-align-memory-on-the-heap-in-a-box-in-a-convenient-way

use std::alloc::*;
use std::num::NonZeroUsize;
use std::ptr::{slice_from_raw_parts_mut, NonNull};

use nix::sys::mman::{mmap_anonymous, munmap, MapFlags, ProtFlags};

pub struct HugePagesAlloc;

#[derive(Debug)]
enum HugePagesAllocFailure {
    MMapErr(nix::errno::Errno),
    Null,
}

fn alloc(size: NonZeroUsize, with_hugepages: bool) -> Result<NonNull<[u8]>, HugePagesAllocFailure> {
    let prot_flags = ProtFlags::PROT_READ | ProtFlags::PROT_WRITE;

    let map_flags = {
        let mut flags = MapFlags::MAP_PRIVATE | MapFlags::MAP_ANONYMOUS;
        if with_hugepages {
            flags |= MapFlags::MAP_HUGETLB;
        }
        flags
    };
    let mmap_result = unsafe { mmap_anonymous(None, size, prot_flags, map_flags) }
        .map_err(HugePagesAllocFailure::MMapErr)?;
    let slc = slice_from_raw_parts_mut(mmap_result.cast::<u8>().as_ptr(), size.get());
    let nonnull = NonNull::new(slc).ok_or(HugePagesAllocFailure::Null)?;
    Ok(nonnull)
}

unsafe impl Allocator for HugePagesAlloc {
    fn allocate(&self, layout: Layout) -> Result<NonNull<[u8]>, AllocError> {
        let size = NonZeroUsize::new(layout.size()).ok_or(AllocError)?;

        for with_hugepages in [true, false] {
            match alloc(size, with_hugepages) {
                Err(_) if with_hugepages => {
                    println!("Could not allocate with hugepages, falling back to regular");
                    continue;
                }
                x => {
                    return x.map_err(|err| {
                        println!("{err:?}");
                        AllocError {}
                    })
                }
            }
        }
        Err(AllocError)
    }
    unsafe fn deallocate(&self, ptr: NonNull<u8>, layout: Layout) {
        munmap(ptr.cast(), layout.size()).expect("munmap failed");
    }
}
