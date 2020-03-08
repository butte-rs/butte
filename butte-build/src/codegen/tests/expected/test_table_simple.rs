pub enum EntryOffset {}
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Entry<B> {
    table: butte::Table<B>,
}
impl<B> From<butte::Table<B>> for Entry<B> {
    fn from(table: butte::Table<B>) -> Self {
        Self { table }
    }
}
impl<B> From<Entry<B>> for butte::Table<B> {
    fn from(s: Entry<B>) -> Self {
        s.table
    }
}
impl<'a> Entry<&'a [u8]> {
    pub fn create<'bldr: 'args, 'args: 'mut_bldr, 'mut_bldr>(
        fbb: &'mut_bldr mut butte::FlatBufferBuilder<'bldr>,
        args: &'args EntryArgs,
    ) -> butte::WIPOffset<Entry<&'bldr [u8]>> {
        let mut builder = EntryBuilder::new(fbb);
        builder.finish()
    }
}
impl<B> Entry<B>
where
    B: std::convert::AsRef<[u8]>,
{
    pub fn get_root(buf: B) -> Result<Self, butte::Error> {
        let table = butte::Table::get_root(buf)?;
        Ok(Self { table })
    }
}
impl<'a> butte::Follow<'a> for Entry<&'a [u8]> {
    type Inner = Self;
    #[inline]
    fn follow(buf: &'a [u8], loc: usize) -> Result<Self::Inner, butte::Error> {
        let table = butte::Table::new(buf, loc);
        Ok(Self { table })
    }
}
impl<B> butte::FollowBuf for Entry<B>
where
    B: std::convert::AsRef<[u8]>,
{
    type Buf = B;
    type Inner = Self;
    #[inline]
    fn follow_buf(buf: Self::Buf, loc: usize) -> Result<Self::Inner, butte::Error> {
        let table = butte::Table::new(buf, loc);
        Ok(Self { table })
    }
}
pub struct EntryArgs {}
impl Default for EntryArgs {
    fn default() -> Self {
        Self {}
    }
}
pub struct EntryBuilder<'a, 'b> {
    fbb: &'b mut butte::FlatBufferBuilder<'a>,
    start: butte::WIPOffset<butte::TableUnfinishedWIPOffset>,
}
impl<'a: 'b, 'b> EntryBuilder<'a, 'b> {
    #[inline]
    pub fn new(fbb: &'b mut butte::FlatBufferBuilder<'a>) -> Self {
        let start = fbb.start_table();
        EntryBuilder { fbb, start }
    }
    #[inline]
    pub fn finish(self) -> butte::WIPOffset<Entry<&'a [u8]>> {
        let o = self.fbb.end_table(self.start);
        butte::WIPOffset::new(o.value())
    }
}
pub enum MyTableOffset {}
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct MyTable<B> {
    table: butte::Table<B>,
}
impl<B> From<butte::Table<B>> for MyTable<B> {
    fn from(table: butte::Table<B>) -> Self {
        Self { table }
    }
}
impl<B> From<MyTable<B>> for butte::Table<B> {
    fn from(s: MyTable<B>) -> Self {
        s.table
    }
}
impl<'a> MyTable<&'a [u8]> {
    pub const VT_ENTRIES: butte::VOffsetT = 4u16;
    pub fn create<'bldr: 'args, 'args: 'mut_bldr, 'mut_bldr>(
        fbb: &'mut_bldr mut butte::FlatBufferBuilder<'bldr>,
        args: &'args MyTableArgs<'args>,
    ) -> butte::WIPOffset<MyTable<&'bldr [u8]>> {
        let mut builder = MyTableBuilder::new(fbb);
        if let Some(x) = args.entries {
            builder.add_entries(x);
        }
        builder.finish()
    }
}
impl<B> MyTable<B>
where
    B: std::convert::AsRef<[u8]>,
{
    #[inline]
    #[allow(clippy::type_complexity)]
    pub fn entries(
        &self,
    ) -> Result<Option<butte::Vector<'_, butte::ForwardsUOffset<Entry<&[u8]>>>>, butte::Error> {
        self.table
            .get::<butte::ForwardsUOffset<butte::Vector<'_, butte::ForwardsUOffset<Entry<&[u8]>>>>>(
                MyTable::VT_ENTRIES,
            )
    }
    pub fn get_root(buf: B) -> Result<Self, butte::Error> {
        let table = butte::Table::get_root(buf)?;
        Ok(Self { table })
    }
}
impl<'a> butte::Follow<'a> for MyTable<&'a [u8]> {
    type Inner = Self;
    #[inline]
    fn follow(buf: &'a [u8], loc: usize) -> Result<Self::Inner, butte::Error> {
        let table = butte::Table::new(buf, loc);
        Ok(Self { table })
    }
}
impl<B> butte::FollowBuf for MyTable<B>
where
    B: std::convert::AsRef<[u8]>,
{
    type Buf = B;
    type Inner = Self;
    #[inline]
    fn follow_buf(buf: Self::Buf, loc: usize) -> Result<Self::Inner, butte::Error> {
        let table = butte::Table::new(buf, loc);
        Ok(Self { table })
    }
}
pub struct MyTableArgs<'a> {
    #[allow(clippy::type_complexity)]
    pub entries: Option<butte::WIPOffset<butte::Vector<'a, butte::WIPOffset<Entry<&'a [u8]>>>>>,
}
impl<'a> Default for MyTableArgs<'a> {
    fn default() -> Self {
        Self { entries: None }
    }
}
pub struct MyTableBuilder<'a, 'b> {
    fbb: &'b mut butte::FlatBufferBuilder<'a>,
    start: butte::WIPOffset<butte::TableUnfinishedWIPOffset>,
}
impl<'a: 'b, 'b> MyTableBuilder<'a, 'b> {
    #[inline]
    #[allow(clippy::type_complexity)]
    fn add_entries(
        &mut self,
        entries: butte::WIPOffset<butte::Vector<'b, butte::WIPOffset<Entry<&'b [u8]>>>>,
    ) {
        self . fbb . push_slot_always :: < butte :: WIPOffset :: < butte :: Vector < 'b , butte :: WIPOffset :: < Entry < & 'b [ u8 ] > > > > > ( MyTable :: VT_ENTRIES , entries ) ;
    }
    #[inline]
    pub fn new(fbb: &'b mut butte::FlatBufferBuilder<'a>) -> Self {
        let start = fbb.start_table();
        MyTableBuilder { fbb, start }
    }
    #[inline]
    pub fn finish(self) -> butte::WIPOffset<MyTable<&'a [u8]>> {
        let o = self.fbb.end_table(self.start);
        self.fbb.required(o, MyTable::VT_ENTRIES, "entries");
        butte::WIPOffset::new(o.value())
    }
}
