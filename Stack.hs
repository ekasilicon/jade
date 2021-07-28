

readVaruint = do
  b <- readByte
  if b & 0x80 == 0
    then unit b
    else
    do
      n <- readVaruint
      return (n * 128) + (b & 0x7F)
        
    
    
