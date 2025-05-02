#include <reg51.h>

/* Пример обработки внешних прерываний INT0 и INT1 */
sbit LED0 = P1^0;    // LED для индикации INT0
sbit LED1 = P1^1;    // LED для индикации INT1

// Состояния светодиодов
unsigned char led0_state = 0;
unsigned char led1_state = 0;

void ext_int_init(void) {
    IT0 = 1;    // Прерывание INT0 по фронту
    IT1 = 1;    // Прерывание INT1 по фронту
    
    EX0 = 1;    // Разрешаем внешнее прерывание 0
    EX1 = 1;    // Разрешаем внешнее прерывание 1
    EA = 1;     // Разрешаем все прерывания
}

// Обработчик прерывания INT0
void ext0_isr(void) interrupt 0 {
    led0_state = !led0_state;
    LED0 = led0_state;
}

// Обработчик прерывания INT1
void ext1_isr(void) interrupt 2 {
    led1_state = !led1_state;
    LED1 = led1_state;
}

void main(void) {
    // Инициализация
    LED0 = 0;
    LED1 = 0;
    ext_int_init();
    
    while(1) {
        // Основной цикл программы пуст,
        // вся работа выполняется в прерываниях
    }
} 