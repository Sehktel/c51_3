/* Множественное включение заголовочных файлов */
#include <reg51.h>
#include "test_07_simple.h"

void main(void) {
    char a;
    test_func();
    P1 = 0x00;
} 