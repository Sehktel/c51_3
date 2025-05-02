#include <reg2051.h>

void main(void){
	unsigned char myArray[32] = {1, 2, 3, 4, 5, 6};
	char i;
	while(1){
		for(i = 0; i<32; i++) {
			P1 = myArray[i];
		}
	}
}
