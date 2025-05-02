/* Внешнее прерывание */
#include <reg51.h>

void ext0_isr(void) interrupt 0 {
    P1 = 0xFF;
}

void main(void) {
    IT0 = 1;    // Edge triggered
    EX0 = 1;    // Enable INT0
    EA = 1;     // Enable all
} 