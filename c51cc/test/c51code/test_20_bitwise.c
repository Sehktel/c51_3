/* Побитовые операции */
void main(void) {
    char a, b, c;
    
    a = 0x0F;        // 0000 1111
    b = 0xF0;        // 1111 0000
    
    c = a & b;       // AND
    c = a | b;       // OR
    c = a ^ b;       // XOR
    c = ~a;          // NOT
    c = a << 2;      // Сдвиг влево
    c = b >> 1;      // Сдвиг вправо
} 