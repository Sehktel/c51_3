/* Работа с банками регистров */
#include <reg51.h>

void func1(void) using 1 {  // Использование банка 1
    char a;
    a = 10;
}

void func2(void) using 2 {  // Использование банка 2
    char b;
    b = 20;
}

void main(void) {
    func1();
    func2();
} 