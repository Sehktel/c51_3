/* Условная компиляция */
#define DEBUG 1

void main(void) {
    char a;
    a = 10;
    
#ifdef DEBUG
    P1 = a;    // Debug output
#endif
}