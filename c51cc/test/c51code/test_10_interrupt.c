/* Простой обработчик прерывания */
#include <reg51.h>

void timer0_isr(void) interrupt 1 {
    P1_0 = !P1_0;
}

void main(void) {
    EA = 1;    // Enable All interrupts
    ET0 = 1;   // Enable Timer 0 interrupt
} 