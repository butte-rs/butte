/*
 * Copyright 2018 Google Inc. All rights reserved.
 * Copyright 2019 Butte authors. All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

use std::{
    iter::{DoubleEndedIterator, ExactSizeIterator, FusedIterator},
    marker::PhantomData,
    mem::size_of,
    slice::from_raw_parts,
    str::from_utf8,
};

use crate::Error;

#[cfg(target_endian = "little")]
use crate::endian_scalar::EndianScalar;
use crate::{endian_scalar::read_scalar_at, follow::Follow, primitives::*};

#[derive(Debug)]
pub struct Vector<'a, T: 'a>(&'a [u8], usize, PhantomData<T>);

// We cannot use derive for these two impls, as it would only implement Copy
// and Clone for `T: Copy` and `T: Clone` respectively. However `Vector<'a, T>`
// can always be copied, no matter that `T` you have.
impl<'a, T> Copy for Vector<'a, T> {}
impl<'a, T> Clone for Vector<'a, T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<'a, T: 'a> Vector<'a, T> {
    #[inline(always)]
    pub fn new(buf: &'a [u8], loc: usize) -> Self {
        Vector {
            0: buf,
            1: loc,
            2: PhantomData,
        }
    }

    #[allow(clippy::len_without_is_empty)]
    #[inline(always)]
    pub fn len(&self) -> Result<usize, Error> {
        Ok(read_scalar_at::<UOffsetT>(self.0, self.1)? as usize)
    }
    #[inline(always)]
    pub fn is_empty(&self) -> Result<bool, Error> {
        Ok(self.len()? == 0)
    }
}

impl<'a, T: Follow<'a> + 'a> Vector<'a, T> {
    #[inline(always)]
    pub fn get(&self, idx: usize) -> Result<T::Inner, Error> {
        debug_assert!(idx < read_scalar_at::<u32>(self.0, self.1)? as usize);
        let sz = size_of::<T>();
        debug_assert!(sz > 0);
        T::follow(self.0, self.1 as usize + SIZE_UOFFSET + sz * idx)
    }

    #[inline(always)]
    pub fn iter(&self) -> VectorIter<'a, T> {
        // This method cannot fail -- we return a len of 0 (an empty iterator)
        // if reading len() returns an error.
        let len = self.len().unwrap_or(0);
        VectorIter::new(*self, len)
    }
}

pub trait SafeSliceAccess {}
impl<'a, T: SafeSliceAccess + 'a> Vector<'a, T> {
    pub fn safe_slice(self) -> Result<&'a [T], Error> {
        let buf = self.0;
        let loc = self.1;
        let sz = size_of::<T>();
        debug_assert!(sz > 0);
        let len = read_scalar_at::<UOffsetT>(buf, loc)? as usize;
        let data_buf = buf
            .get(loc + SIZE_UOFFSET..loc + SIZE_UOFFSET + len * sz)
            .ok_or(Error::OutOfBounds)?;
        let ptr = data_buf.as_ptr() as *const T;
        let s: &'a [T] = unsafe { from_raw_parts(ptr, len) };
        Ok(s)
    }
}

impl SafeSliceAccess for u8 {}
impl SafeSliceAccess for i8 {}

#[cfg(target_endian = "little")]
mod le_safe_slice_impls {
    impl super::SafeSliceAccess for u16 {}
    impl super::SafeSliceAccess for u32 {}
    impl super::SafeSliceAccess for u64 {}

    impl super::SafeSliceAccess for i16 {}
    impl super::SafeSliceAccess for i32 {}
    impl super::SafeSliceAccess for i64 {}

    impl super::SafeSliceAccess for f32 {}
    impl super::SafeSliceAccess for f64 {}
}

#[cfg(target_endian = "little")]
pub use self::le_safe_slice_impls::*;

/// Follow the pointer to the Vector payload, and cast it to T
///
/// # Safety
///
/// A pointer to T must be safely cast from any bytes and thus
/// have the right representation, alignment, ...
pub unsafe fn follow_cast_ref<'a, T: Sized + 'a>(
    buf: &'a [u8],
    loc: usize,
) -> Result<&'a T, Error> {
    let sz = size_of::<T>();
    let buf = buf.get(loc..loc + sz).ok_or(Error::OutOfBounds)?;
    let ptr = buf.as_ptr() as *const T;
    Ok(&*ptr)
}

impl<'a> Follow<'a> for &'a str {
    type Inner = &'a str;
    fn follow(buf: &'a [u8], loc: usize) -> Result<Self::Inner, Error> {
        // Len of the String/Vector
        let len = read_scalar_at::<UOffsetT>(buf, loc)? as usize;

        let end_loc = loc + SIZE_UOFFSET + len;

        // We check bounds early, because we need to make sure strings are null-terminated
        // The null byte not included in the string `len`
        if end_loc >= buf.len() {
            return Err(Error::OutOfBounds);
        }
        // This is effectively the cell *after* the String len
        if buf[end_loc] != 0 {
            return Err(Error::NonNullTerminatedString);
        }

        let slice = &buf[loc + SIZE_UOFFSET..end_loc];

        // Strings are just byte vectors in Flatbuffers -- so one should use `&[u8]`
        // for compatibility with other systems, and `&str` to enforce the payload is valid UTF8
        from_utf8(slice).map_err(|_| Error::NonUtf8String)
    }
}

#[cfg(target_endian = "little")]
fn follow_slice_helper<T>(buf: &[u8], loc: usize) -> Result<&[T], Error> {
    let sz = size_of::<T>();
    debug_assert!(sz > 0);
    let len = read_scalar_at::<UOffsetT>(buf, loc)? as usize;
    let data_buf = buf
        .get(loc + SIZE_UOFFSET..loc + SIZE_UOFFSET + len * sz)
        .ok_or(Error::OutOfBounds)?;
    let ptr = data_buf.as_ptr() as *const T;
    let s: &[T] = unsafe { from_raw_parts(ptr, len) };
    Ok(s)
}

/// Implement direct slice access if the host is little-endian.
#[cfg(target_endian = "little")]
impl<'a, T: EndianScalar> Follow<'a> for &'a [T] {
    type Inner = &'a [T];
    fn follow(buf: &'a [u8], loc: usize) -> Result<Self::Inner, Error> {
        follow_slice_helper::<T>(buf, loc)
    }
}

/// Implement Follow for all possible Vectors that have Follow-able elements.
impl<'a, T: Follow<'a> + 'a> Follow<'a> for Vector<'a, T> {
    type Inner = Vector<'a, T>;
    fn follow(buf: &'a [u8], loc: usize) -> Result<Self::Inner, Error> {
        Ok(Vector::new(buf, loc))
    }
}

#[derive(Debug)]
pub struct VectorIter<'a, T: 'a> {
    buf: &'a [u8],
    loc: usize,
    remaining: usize,
    phantom: PhantomData<T>,
}

impl<'a, T: 'a> VectorIter<'a, T> {
    #[inline]
    pub fn new(inner: Vector<'a, T>, len: usize) -> Self {
        VectorIter {
            buf: inner.0,
            // inner.1 is the location of the data for the vector.
            // The first SIZE_UOFFSET bytes is the length. We skip
            // that to get to the actual vector content.
            loc: inner.1 + SIZE_UOFFSET,
            remaining: len,
            phantom: PhantomData,
        }
    }
}

impl<'a, T: Follow<'a> + 'a> Clone for VectorIter<'a, T> {
    #[inline]
    fn clone(&self) -> Self {
        VectorIter {
            buf: self.buf,
            loc: self.loc,
            remaining: self.remaining,
            phantom: self.phantom,
        }
    }
}

impl<'a, T: Follow<'a> + 'a> Iterator for VectorIter<'a, T> {
    type Item = Result<T::Inner, Error>;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        let sz = size_of::<T>();
        debug_assert!(sz > 0);

        if self.remaining == 0 {
            None
        } else {
            let result = T::follow(self.buf, self.loc);
            self.loc += sz;
            self.remaining -= 1;
            Some(result)
        }
    }

    #[inline]
    fn nth(&mut self, n: usize) -> Option<Self::Item> {
        let sz = size_of::<T>();
        debug_assert!(sz > 0);

        self.remaining = self.remaining.saturating_sub(n);

        // Note that this might overflow, but that is okay because
        // in that case self.remaining will have been set to zero.
        self.loc = self.loc.wrapping_add(sz * n);

        self.next()
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.remaining, Some(self.remaining))
    }
}

impl<'a, T: Follow<'a> + 'a> DoubleEndedIterator for VectorIter<'a, T> {
    #[inline]
    fn next_back(&mut self) -> Option<Self::Item> {
        let sz = size_of::<T>();
        debug_assert!(sz > 0);

        if self.remaining == 0 {
            None
        } else {
            self.remaining -= 1;
            Some(T::follow(self.buf, self.loc + sz * self.remaining))
        }
    }

    #[inline]
    fn nth_back(&mut self, n: usize) -> Option<Self::Item> {
        self.remaining = self.remaining.saturating_sub(n);
        self.next_back()
    }
}

impl<'a, T: 'a + Follow<'a>> ExactSizeIterator for VectorIter<'a, T> {
    #[inline]
    fn len(&self) -> usize {
        self.remaining
    }
}

impl<'a, T: 'a + Follow<'a>> FusedIterator for VectorIter<'a, T> {}

impl<'a, T: Follow<'a> + 'a> IntoIterator for Vector<'a, T> {
    type Item = Result<T::Inner, Error>;
    type IntoIter = VectorIter<'a, T>;
    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<'a, 'b, T: Follow<'a> + 'a> IntoIterator for &'b Vector<'a, T> {
    type Item = Result<T::Inner, Error>;
    type IntoIter = VectorIter<'a, T>;
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}
