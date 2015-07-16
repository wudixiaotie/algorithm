-module (make_md5).

-export ([md5_octet_16/1, md5_hex_32/1]).

md5_hex_16(Bin) ->
    erlang:md5(Bin).

md5_hex_32(Bin) ->
    MD5_16 = md5_hex_16(Data),
    << <<(hex(A)), (hex(B))>> || <<A:4,B:4>> <= MD5_16 >>.


hex(0)  -> $0;
hex(1)  -> $1;
hex(2)  -> $2;
hex(3)  -> $3;
hex(4)  -> $4;
hex(5)  -> $5;
hex(6)  -> $6;
hex(7)  -> $7;
hex(8)  -> $8;
hex(9)  -> $9;
hex(10) -> $a;
hex(11) -> $b;
hex(12) -> $c;
hex(13) -> $d;
hex(14) -> $e;
hex(15) -> $f.