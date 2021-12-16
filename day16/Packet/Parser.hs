module Packet.Parser where

import Packet
import Bit

litType :: Integral a => a
litType = 4

parse_literal :: Integer -> Bits -> (Integer,Packet,Bits)
parse_literal version stream =
    let (numbits,digits,rem) = get_digit 0 [] stream in
        (6 + numbits, PLiteral (fromInteger version) (get_num digits), rem)
    where get_digit numbits acc stream =
              let ([i,hd],rem) = take_parse [1,4] stream in
                  if i == 0 then (numbits + 5, hd:acc,rem)
                            else get_digit (numbits + 5) (hd:acc) rem
          get_num = foldr (\x acc -> acc * 16 + x) 0

parse_optype :: Integer -> OpType
parse_optype 0 = Sum
parse_optype 1 = Product
parse_optype 2 = Min
parse_optype 3 = Max
parse_optype 5 = GreaterThan
parse_optype 6 = LessThan
parse_optype 7 = EqualsTo

parse_operator :: Integer -> Integer -> Bits -> (Integer,Packet,Bits)
parse_operator typint version stream =
    let (numbits,sp,rem) = parse_subpackets stream in
        (6 + numbits, POperator (fromInteger version) (parse_optype typint) sp, rem)

parse_subpackets :: Bits -> (Integer,[Packet],Bits)
parse_subpackets (B0:stream) =
    let ([size],rem) = take_parse [15] stream in
        let (numbits,packs,rem') = parse_packets_size size rem in
            (numbits + 16,packs,rem')
parse_subpackets (B1:stream) =
    let ([num],rem) = take_parse [11] stream in
        let (numbits,packs,rem') = parse_packets_num num rem in
            (numbits + 12,packs,rem')

parse_packets_size :: Integer -> Bits -> (Integer,[Packet],Bits)
parse_packets_size max stream =
    parse_next 0 [] stream
    where parse_next numbits packs stream 
              | numbits >= max = (numbits, reverse packs, stream)
              | otherwise =
                  let (ns,p,rem) = parse_packet stream in
                      parse_next (numbits + ns) (p:packs) rem 

parse_packets_num :: Integer -> Bits -> (Integer,[Packet],Bits)
parse_packets_num 0 stream = (0,[],stream)
parse_packets_num n stream =
    let (numbits,pack,rem) = parse_packet stream in
        let (numbits',packs,rem') = parse_packets_num (n - 1) rem in
            (numbits + numbits',pack:packs,rem')

parse_packet :: Bits -> (Integer,Packet,Bits)
parse_packet stream =
    let ([ver,typ],rem) = take_parse [3,3] stream in
        dispatch typ ver rem
    where dispatch typ ver stream
              | typ == litType = parse_literal ver stream
              | otherwise = parse_operator typ ver stream



