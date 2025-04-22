void Timer0_ISR(void) interrupt 1 {
    while(1) {
        P1 = 0x55;
        P2 = 0xAA;
    }
} 