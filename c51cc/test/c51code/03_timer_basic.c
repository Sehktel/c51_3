#include <reg51.h>

/* Базовая работа с таймером 0 в режиме 1 (16-бит) */
sbit OUTPUT_PIN = P1^0;

// Константы для таймера (1мс при 12 МГц)
#define TIMER_H_INIT 0xFC
#define TIMER_L_INIT 0x18

void timer0_init(void) {
    TMOD &= 0xF0;    // Очищаем биты режима таймера 0
    TMOD |= 0x01;    // Устанавливаем режим 1
    
    TH0 = TIMER_H_INIT;  // Загружаем начальное значение
    TL0 = TIMER_L_INIT;
    
    ET0 = 1;    // Разрешаем прерывание от таймера 0
    EA = 1;     // Разрешаем все прерывания
    TR0 = 1;    // Запускаем таймер
}

// Обработчик прерывания таймера
void timer0_isr(void) interrupt 1 {
    TH0 = TIMER_H_INIT;  // Перезагружаем значения
    TL0 = TIMER_L_INIT;
    
    OUTPUT_PIN = !OUTPUT_PIN;  // Инвертируем выход
}

void main(void) {
    OUTPUT_PIN = 0;
    timer0_init();
    
    while(1) {
        // Основной цикл программы
    }
} 