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

use crate::{
    follow::{Follow, FollowBuf},
    primitives::*,
    vtable::VTable,
    Error,
};

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Table<B> {
    pub buf: B,
    pub loc: usize,
}

impl<B> Table<B> {
    #[inline]
    pub fn new(buf: B, loc: usize) -> Self {
        Table { buf, loc }
    }
}

impl<B> Table<B>
where
    B: std::convert::AsRef<[u8]>,
{
    pub fn get_root(buf: B) -> Result<Self, Error> {
        Ok(<ForwardsUOffset<Self>>::follow_buf(buf, 0)?)
    }

    pub fn get_size_prefixed_root(buf: B) -> Result<Self, Error> {
        Ok(<SkipSizePrefix<ForwardsUOffset<Self>>>::follow_buf(buf, 0)?)
    }

    pub fn vtable<'a>(&'a self) -> Result<VTable<'a>, Error> {
        <BackwardsSOffset<VTable<'a>>>::follow(self.buf.as_ref(), self.loc)
    }

    #[inline]
    pub fn get<'a, T: Follow<'a> + 'a>(
        &'a self,
        slot_byte_loc: VOffsetT,
        default: Option<T::Inner>,
    ) -> Result<Option<T::Inner>, Error> {
        let v_offset = self.vtable()?.get(slot_byte_loc)?;
        if let Some(v_offset) = v_offset {
            let offset = v_offset as usize;
            if offset == 0 {
                return Ok(default);
            }
            <T>::follow(self.buf.as_ref(), self.loc + offset).map(Some)
        } else {
            Ok(default)
        }
    }
}

impl<B> FollowBuf for Table<B>
where
    B: std::convert::AsRef<[u8]>,
{
    type Buf = B;
    type Inner = Table<B>;
    #[inline]
    fn follow_buf(buf: Self::Buf, loc: usize) -> Result<Self::Inner, Error> {
        Ok(Table::new(buf, loc))
    }
}

#[inline]
pub fn get_root<T, B>(data: B) -> Result<T::Inner, Error>
where
    T: FollowBuf<Buf = B>,
    B: std::convert::AsRef<[u8]>,
{
    <ForwardsUOffset<T>>::follow_buf(data, 0)
}
#[inline]
pub fn get_size_prefixed_root<T, B>(data: B) -> Result<T::Inner, Error>
where
    T: FollowBuf<Buf = B>,
    B: std::convert::AsRef<[u8]>,
{
    <SkipSizePrefix<ForwardsUOffset<T>>>::follow_buf(data, 0)
}
#[inline]
pub fn buffer_has_identifier(data: &[u8], ident: &str, size_prefixed: bool) -> Result<bool, Error> {
    assert_eq!(ident.len(), FILE_IDENTIFIER_LENGTH);

    let got = if size_prefixed {
        <SkipSizePrefix<SkipRootOffset<FileIdentifier>>>::follow(data, 0)?
    } else {
        <SkipRootOffset<FileIdentifier>>::follow(data, 0)?
    };

    Ok(ident.as_bytes() == got)
}
