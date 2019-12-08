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

use crate::{follow::Follow, primitives::*, vtable::VTable, Error};

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Table<'a> {
    pub buf: &'a [u8],
    pub loc: usize,
}

impl<'a> Table<'a> {
    #[inline]
    pub fn new(buf: &'a [u8], loc: usize) -> Self {
        Table { buf, loc }
    }
    #[inline]
    pub fn vtable(&self) -> Result<VTable<'a>, Error> {
        <BackwardsSOffset<VTable<'a>>>::follow(self.buf, self.loc)
    }
    #[inline]
    pub fn get<T: Follow<'a> + 'a>(
        &self,
        slot_byte_loc: VOffsetT,
    ) -> Result<Option<T::Inner>, Error> {
        let v_offset = self.vtable()?.get(slot_byte_loc)?;
        if let Some(v_offset) = v_offset {
            let o = v_offset as usize;
            <T>::follow(self.buf, self.loc + o).map(Some)
        } else {
            Ok(None)
        }
    }
}

impl<'a> Follow<'a> for Table<'a> {
    type Inner = Table<'a>;
    #[inline]
    fn follow(buf: &'a [u8], loc: usize) -> Result<Self::Inner, Error> {
        Ok(Table { buf, loc })
    }
}

#[inline]
pub fn get_root<'a, T: Follow<'a> + 'a>(data: &'a [u8]) -> Result<T::Inner, Error> {
    <ForwardsUOffset<T>>::follow(data, 0)
}
#[inline]
pub fn get_size_prefixed_root<'a, T: Follow<'a> + 'a>(data: &'a [u8]) -> Result<T::Inner, Error> {
    <SkipSizePrefix<ForwardsUOffset<T>>>::follow(data, 0)
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
