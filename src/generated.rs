macro_rules! import_generated {
    ($($name:ident),*) => {
        $(
            pub mod $name {
                include!(concat!(env!("OUT_DIR"), "/", stringify!($name), ".rs"));
            }
        )*
    }
}

import_generated! {hashes}
import_generated! {squares}
import_generated! {magics}
import_generated! {parameters}
