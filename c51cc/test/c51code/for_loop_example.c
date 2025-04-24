void main(void) {
    unsigned char i;
    unsigned char sum;
    sum = 0;
    for (i = 1; i <= 10; i++) {
        sum += i;
        sum = sum + 2;
        sum -= 1;
    }
}
