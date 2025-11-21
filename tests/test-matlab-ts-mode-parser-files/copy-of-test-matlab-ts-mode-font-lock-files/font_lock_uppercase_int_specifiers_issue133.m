% -*- matlab-ts -*-

bin1_int16 = [0b100000s16, 0b011111s16];
bin2_int16 = [0B100000S16, 0B011111S16];
isequal(bin1_int16, bin2_int16)

bin1_uint16 = [0b100000u16, 0b011111u16];
bin2_uint16 = [0B100000U16, 0B011111U16];
isequal(bin1_uint16, bin2_uint16)
