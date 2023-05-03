nekofs: create and extract nekodata files

Usage: nekofs ((-x|--extract NEKOFILE) [OUTDIR] [-v|--verify] |
                (-c|--create SRCDIR) [OUTFILE])

Available options:
  -x,--extract NEKOFILE    The .nekodata file to extract
  OUTDIR                   The output directory (default: "output")
  -v,--verify              verify integrity after extraction
  -c,--create SRCDIR       Source directory to be packed into a .nekodata file
  OUTFILE                  The output file name (default: SRCDIR.nekodata)
  -h,--help                Show this help text

Additionally, use the shell auto-completion script:

    $ source <(nekofs --bash-completion-script `which nekofs`)

  which also analogously applies to zsh and fish


--------------------------------------------------------------------

As my understand, the nekodata file is structured like this:


    ┌────────────────────────────────────────────────┐
    │                                                │
    │  Header (length = 25)                          │
    │                                                │
    │    Magic  ('pixelneko filesystem')             │
    │    Word32 (0x00000000)                         │
    │    Word8  (0x01), unknown usage                │
    │                                                │
    ├────────────────────────────────────────────────┤
    │                                                │
    │  File #0                                       │
    │                                                │
    │    ┌──────────────────────────────────────┐    │
    │    │                                      │    │
    │    │  Raw LZ4 Data Block #0 (without FD)  │    │
    │    │                                      │    │
    │    ├──────────────────────────────────────┤    │
    │    │                                      │    │
    │    │  Raw LZ4 Data Block #1               │    │
    │    │                                      │    │
    │    ├──────────────────────────────────────┤    │
    │    │                                      │    │
    │    │  ...                                 │    │
    │    │                                      │    │
    │    └──────────────────────────────────────┘    │
    │                                                │
    ├────────────────────────────────────────────────┤
    │                                                │
    │  File #1                                       │
    │                                                │
    ├────────────────────────────────────────────────┤
    │                                                │
    │  ...                                           │
    │                                                │
    ├────────────────────────────────────────────────┤
    │                                                │
    │  Metadata (encrypted with ChaCha20 cipher)     │
    │                                                │
    │    Entry Count        :: ShiftedVLQ            │
    │                                                │
    │  ┌──────────────────────────────────────────┐  │
    │  │                                          │  │
    │  │  Entry #0                                │  │
    │  │                                          │  │
    │  │    Filename Length       :: ShiftedVLQ   │  │
    │  │    Filename              :: String       │  │
    │  │    Word8 (0x02), unknown usage           │  │
    │  │    File Size             :: ShiftedVLQ   │  │
    │  │    Total BlockSize       :: ShiftedVLQ   │  │
    │  │    CRC32                 :: IntVLQ       │  │
    │  │    Offset of first Block :: IntVLQ       │  │
    │  │    Block Counts          :: ShiftedVLQ   │  │
    │  │                                          │  │
    │  │  ┌────────────────────────────────────┐  │  │
    │  │  │                                    │  │  │
    │  │  │  Block Info #0  (always 0,0)       │  │  │
    │  │  │                                    │  │  │
    │  │  │    Offset from 1st block           │  │  │
    │  │  │             :: ShiftedVLQ          │  │  │
    │  │  │    Offset in the original file     │  │  │
    │  │  │             :: ShiftedVLQ          │  │  │
    │  │  │                                    │  │  │
    │  │  ├────────────────────────────────────┤  │  │
    │  │  │                                    │  │  │
    │  │  │  Block Info #1  (_, 32768*1)       │  │  │
    │  │  │                                    │  │  │
    │  │  ├────────────────────────────────────┤  │  │
    │  │  │                                    │  │  │
    │  │  │  ...                               │  │  │
    │  │  │                                    │  │  │
    │  │  └────────────────────────────────────┘  │  │
    │  │                                          │  │
    │  ├──────────────────────────────────────────┤  │
    │  │                                          │  │
    │  │  Entry #1                                │  │
    │  │                                          │  │
    │  ├──────────────────────────────────────────┤  │
    │  │                                          │  │
    │  │  ...                                     │  │
    │  │                                          │  │
    │  └──────────────────────────────────────────┘  │
    │                                                │
    ├────────────────────────────────────────────────┤
    │                                                │
    │  Length of Metadata :: ShiftedVLQ  (reversed)  │
    │                                                │
    └────────────────────────────────────────────────┘

where the VLQ stands for Variable Length Quantity (little-endian), and the ShiftedVLQ means the original value is rotated left by 1 before being turned into a VLQ.

