# KLang programming language

## Идея
Создать полностью чистый и строгий язык программирования.
Все конструкции по типу else, elseif, while, for и т.д. должны были бы реализовываться через функции.
Также ЯП не поддерживает скобки, но это является его преимуществом, так как обязывает программиста
придумывать название для каждой переменной, а значит и обосновывать назначение каждой операции.

## Технологии и архитектура.
Для реализации парсера используется библиотека FParsec.
После парсинга файла с исходным кодом, построенное синтаксическое дерево передается на обработку интерпретатору.
Интерпрератор, проходя по синтаксическому дереву, для начала считывает значения переменных из текущего состояния окружения там где это требуется (evaluation), и, затем, передает в обработку (applying), где уже и происходит выполнение нужной логики.

## Статус
1) Реализован парсер языка программирования;
2) Есть проблемы с реализацией интерпрератора языка - работа встала на фиксе конструкции return, которая должна была бы возвращать результат по дереву вверх;
3) Есть успех, связанный с успешной работой рекурсии. Можно обнаружить в режиме отладки, что факториал все-таки расчитывается;
4) Поддерживаются базовые бинарные операции и сравнения.

## Структура
- examples - примеры программ (реализован расчет факториала)
- Parser.fs - парсер языка KLang
- Program.fs - интерпретатор языка KLang.

## Общий вывод.
Язык программирования готов примерно на 80% - оставалось только поправить проблему с return и добавить пару функций стандартной библиотеки. Тогда в итоге мы бы получили тьюринг-полный язык, так как он поддерживает рекурсию и сравнения и, соответственно, благодаря им, можно было бы реализовать и вычислить любую функцию - к примеру, реализовать цикл for. 

## Контакты автора.
Реализовал студент Митюшкин Максим группы БПИ216. 
По возникшим вопросам можно обратиться в Telegram - @kotlinovsky.