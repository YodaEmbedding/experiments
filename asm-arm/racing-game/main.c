
#include "axi_oled_controller.h"
#include "xparameters.h"
#include "xgpio.h"
#include "xscugic.h"
#include "xil_exception.h"

extern void asm_main();


/*
void MT_Initialize(unsigned int seed);
unsigned int MT_Twist();
unsigned int MT_Extract();
*/


int main() {
	/*
	 XStatus test = AXI_OLED_CONTROLLER_Mem_SelfTest((void*)0x41230000);
	while(test == XST_FAILURE)
		;
	*/
	// char *arr = "abc";
	//*((char*)(0x41230000 + 0x10405)) = 'a';

	// MT_Initialize((unsigned int)1234);
	asm_main();

	return 0;
}


/*
// MT19937 Random number generator
// Taken from https://en.wikipedia.org/wiki/Mersenne_Twister

unsigned int x[624];
int MT_index;

void MT_Initialize(unsigned int seed)
{
    MT_index = 624;
    signed int i = 1;
    *x = seed;
    unsigned int *j = x;
    unsigned int _a; int _b;
    do
    {
        _a = i + 1812433253 * (*j ^ (*j >> 30));
        *(j + 1) = _a;
        _b = i + 1812433253 * (_a ^ (_a >> 30)) + 1;
        i += 2;
        *(j + 2) = _b;
        j += 2;
    } while (j < x + 0x26C);
}

unsigned int MT_Twist()
{
    signed int top = 397, l = 623;
    unsigned int *j = x;
    int i; unsigned int _c, out; signed int _f;
    do
    {
        i = (top - 396) % 624;
        _c = (*j ^ (*j ^ (x[i])) & 0x7FFFFFFF) >> 1;
        if ((*j ^ (*j ^ x[i])) & 1)
            _c ^= 0x9908B0DFu;
        _f = top++;
        out = _c ^ x[_f % 624];
        *j = out;
        ++j;
        --l;
    } while (l);
    MT_index = 0;
    return out;
}

unsigned int MT_Extract()
{
    int i = MT_index;
    if (MT_index >= 624)
    {
        MT_Twist();
        i = MT_index;
    }
    unsigned int e = x[i];
    unsigned int _v = x[i] >> 11;
    MT_index = i + 1;
    int def = (((_v ^ e) & 0xFF3A58AD) << 7) ^ _v ^ e;
    return ((def & 0xFFFFDF8C) << 15) ^ def ^ ((((def & 0xFFFFDF8C) << 15) ^ def) >> 18);
}
*/

