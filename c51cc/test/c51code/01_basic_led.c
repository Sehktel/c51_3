#include <reg51.h>

/* Базовый пример мигания светодиодом на порту P1.0 */
sbit LED = P1^0;  // Определяем бит для светодиода на P1.0

void delay(unsigned int count) {
    unsigned int i;
    for(i = 0; i < count; i++);
}

void main(void) {
    while(1) {
        LED = 0;    // Включаем светодиод (активный низкий уровень)
        delay(10000);
        LED = 1;    // Выключаем светодиод
        delay(10000);
    }
} 