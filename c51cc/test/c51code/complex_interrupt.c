void Complex_Interrupt(void) interrupt 2 {
    unsigned char temp = 0;
    while(1) {
        temp++;
        P1 = temp;
        if(temp > 100) {
            temp = 0;
        }
    }
} 