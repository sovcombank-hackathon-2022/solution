## Структура кода

Весь код и бэкенда и фронтенда написан на Common Lisp с использованием JSON-RPC и вебфреймворка [Reblocs](https://40ants.com/reblocks/). Этот язык был выбран, как наиболее эффективный для решения нечётко поставленных задач в кратчайшие сроки. Благодаря возможностям интерактивного программирования и отладки, Common Lisp позволяет одному человеку или небольшой команде достигать существенных результатов. Кому интересно, как происходит разработка, может посмотреть [записи трансляций](https://www.twitch.tv/svetlyak40wt) с хакатона на Twitch.



### Микросервисы

Репозиторий содержит код микросервисов, разложенных по папкам:

* passport - управляет информацией о пользователях.
* accounts - хранит информаци о счетах пользователей и оперциях над ними.
* processing - обрабатывает заявки на покупку продажу валют.
* rates - хранит исторические данные, отдаёт текущие котировки. Так же тут есть интерфейс для подключения новых источников котировок. Пока реализован один источник, который эмулирует изменение котировки в заданном коридоре. Для подключения новых источников достаточно реализовать всего несколько методов.

Каждый сервис работает по `JSON-RPC` и описывается `OpenRPC` спекой. Спеку сервис может отдавать с URL `/openrpc.json` и так же, для того чтобы спеки были доступны на этапе компиляции, они лежат в виде JSON файлов в папке `specs/`. Если скормить URL со спекой https://playground.open-rpc.org/, то можно будет увидеть документацию по API. Но порты микросервисов закрыты от внешнего мира.


### Фронтенд

Код фронтенда лежит в папке `/frontend`.


## Сборка проекта

Микросервисы собираются в docker образы. Сборка запускается одной единстенной командой `make`.

Дальше, каждый сервис нужно запустить либо локально, либо в облаке. Сервисы MVP запущены локально.

Каждый сервис требует следующих переменных окружения:

- JWT_SECRET - любая секретная строка, которая будет ключём для подписи и проверки JWT токенов.
- ENVIRONMENT - по-умолчанию development. Влияет на уровень логгирования, при development логгируется debug, а в остальных случаях только warn и error.
- DB_HOST - хост постгреса.
- DB_PORT - его пора (если отличается).
- DB_USER - имя пользователя (по-умолчанию postgres).
- DB_PASSWORD - пароль.

## Разработка

Основные инструменты для разработки:

* [SBCL](http://www.sbcl.org/manual/) - OpenSource реализация Common Lisp. Если нужна поддержка, то лучше купить коммерческую реализацию [LispWorks](http://www.lispworks.com/).
* [Roswell](https://github.com/roswell/roswell) - менеджер реализаций Common Lisp. С его помощью можно быстро установить SBCL, а так же собирать бинарники для деплоя в продакшн.
* [Qlot](https://github.com/fukamachi/qlot) - сборщик библиотек в виртуальное окружение. Используется для создания воспроизводимых сборок и фиксирования версий используемых библиотек. Это важно для создания стабильного продукта.
* [Emacs](https://www.gnu.org/software/emacs/) + [SLY](https://github.com/joaotavora/sly) - как IDE для разработки. Есть так же альтернативные решения на базе редакторов Atom и VSCode, а так же отдельные коммерческие IDE от LispWorks или Allegro.

## База данных

Мы используем Postgres, так как он достаточно популярен и его легко обслуживать.

Для каждого микросервиса была создана отдельная база в Managed Postgres от Yandex Cloud.

Схемы данных и миграции к ним, лежат в файлах `scheme.sql` внутри папок каждого из микросервисов.
