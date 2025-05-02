#include <reg51.h>

/* Демонстрация битовых операций для 8051 */
sbit TEST_BIT = P1^1;    // Тестовый бит на P1.1
sbit OUTPUT_BIT = P1^2;  // Выходной бит на P1.2

void main(void) {
    unsigned char data_byte = 0x55;  // 0101 0101
    
    // Битовые операции
    data_byte |= 0x80;    // Установка старшего бита
    data_byte &= 0x7F;    // Сброс старшего бита
    data_byte ^= 0xFF;    // Инверсия всех битов
    
    while(1) {
        if(TEST_BIT == 1) {
            OUTPUT_BIT = 1;
            P2 = data_byte;  // Вывод результата на порт P2
        } else {
            OUTPUT_BIT = 0;
            P2 = ~data_byte; // Инвертированный вывод
        }
    }
} 