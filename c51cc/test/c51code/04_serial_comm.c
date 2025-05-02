#include <reg51.h>

/* Пример работы с UART на скорости 9600 бод при частоте 11.0592 МГц */

#define BUFFER_SIZE 16
unsigned char rx_buffer[BUFFER_SIZE];
unsigned char rx_index = 0;

void uart_init(void) {
    SCON = 0x50;    // Режим 1, прием разрешен
    TMOD &= 0x0F;   // Очищаем биты таймера 1
    TMOD |= 0x20;   // Таймер 1 в режиме автоперезагрузки
    
    TH1 = 0xFD;     // Значение для 9600 бод при 11.0592 МГц
    TR1 = 1;        // Запускаем таймер
    
    ES = 1;         // Разрешаем прерывание от UART
    EA = 1;         // Разрешаем все прерывания
}

void uart_send_char(unsigned char c) {
    SBUF = c;           // Загружаем байт для передачи
    while(!TI);         // Ждем окончания передачи
    TI = 0;            // Сбрасываем флаг передачи
}

void uart_send_string(unsigned char *str) {
    while(*str) {
        uart_send_char(*str++);
    }
}

// Обработчик прерывания UART
void uart_isr(void) interrupt 4 {
    if(RI) {
        RI = 0;                    // Сбрасываем флаг приема
        rx_buffer[rx_index] = SBUF;// Сохраняем принятый байт
        
        if(rx_index < BUFFER_SIZE - 1) {
            rx_index++;
        }
    }
}

void main(void) {
    uart_init();
    uart_send_string("8051 UART Test\r\n");
    
    while(1) {
        // Эхо полученных данных
        if(rx_index > 0) {
            uart_send_char(rx_buffer[--rx_index]);
        }
    }
} 