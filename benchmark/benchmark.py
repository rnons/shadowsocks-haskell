#!/usr/bin/env python
import hashlib
import string
import struct

aPoem = "First they came for the Socialists, and I did not speak out-- Because I was not a Socialist.  Then they came for the Trade Unionists, and I did not speak out-- Because I was not a Trade Unionist. Then they came for the Jews, and I did not speak out-- Because I was not a Jew. Then they came for me--and there was no one left to speak for me."

def get_table(key):
    m = hashlib.md5()
    m.update(key)
    s = m.digest()
    (a, b) = struct.unpack('<QQ', s)
    table = [c for c in string.maketrans('', '')]
    for i in xrange(1, 1024):
        table.sort(lambda x, y: int(a % (ord(x) + i) - a % (ord(y) + i)))
    return table

encrypt_table = ''.join(get_table("Don't panic!"))
decrypt_table = string.maketrans(encrypt_table, string.maketrans('', ''))

encrypted = string.translate(aPoem, encrypt_table)
decrypted = string.translate(encrypted, decrypt_table)
print aPoem == decrypted
