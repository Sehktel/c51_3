/* Работа с битами */
#include <reg51.h>

sbit TEST_PIN = P1^0;
sbit LED_PIN = P1^1;

void main(void) {
    bit flag;
    flag = 1;
    TEST_PIN = flag;
    LED_PIN = !TEST_PIN;
} 