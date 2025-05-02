/* Макросы с параметрами */
#define ADD(x,y) ((x) + (y))
#define SET_BIT(reg,bit) ((reg) |= (1 << (bit)))

void main(void) {
    char a, b;
    a = ADD(1, 2);
    b = 0;
    SET_BIT(b, 3);
} 