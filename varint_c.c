#include <stdio.h>

unsigned int
decodeVarIntReal(const char* bytestring, const int str_len, unsigned int* result)
{
	if (str_len < 1) {
		return 0;
	}
	unsigned int value = bytestring[0];
	if (value < 0x80) {
		*result = value;
		return 1;
	}
	value &= 0x7F;
	unsigned int bit_offset = 7;
	unsigned int byte_pos;

	for(byte_pos = 1; byte_pos < str_len; byte_pos++) {
		unsigned char next = bytestring[byte_pos];
		unsigned char this_val = next & 0x7F;
		value = value | (this_val << bit_offset);
		if (next >= 0x80) {
			//printf("next\n");
			bit_offset += 7;
		} else {
			//printf("*result\n");
			*result = value;
			return byte_pos+1;
		}
	}
	return 0;
}

unsigned int
decodeVarInt(const char* bytestring, const int str_len, unsigned int* result)
{
	int count = 1000000;
	unsigned int res;
	while (count-- > 0) {
		res = decodeVarIntReal(bytestring, str_len, result);
	}
	return res;
}


unsigned int
decodeVarInt2(const char* bytestring, const int str_len, unsigned int* result1, unsigned int* result2)
{
	unsigned int status1 = decodeVarInt(bytestring, str_len, result1);
	if (status1 > 0) {
		unsigned int status2 = decodeVarInt(bytestring+status1, str_len-status1, result2);
		if (status2 > 0)
		{
			return status1 + status2;
		} else {
			return 0;
		}
	} else {
		return 0;
	}
}
