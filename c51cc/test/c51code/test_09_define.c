#include <reg51.h> // Надо определить в какой регистр входит P1
// и более того, задать бит P1.0

/* Простые директивы препроцессора */
#define MAX_VALUE 100
#define MIN_VALUE 0
#define LED P1_0

void main(void) {
    char a;
    a = MAX_VALUE;
    if(a > MIN_VALUE) {
        LED = 0;
    }
} 