all:
	# базовый слой для всех сервисов, с библиотеками-зависимостями
	docker build -t svetlyak40wt/sovcomtrade-2022-base:latest -f docker/Dockerfile.base .
	# Сервисы
	docker build -t svetlyak40wt/sovcomtrade-2022-rates:latest -f docker/Dockerfile --build-arg APP=rates .
	docker build -t svetlyak40wt/sovcomtrade-2022-passport:latest -f docker/Dockerfile --build-arg APP=passport .
	docker build -t svetlyak40wt/sovcomtrade-2022-accounts:latest -f docker/Dockerfile --build-arg APP=accounts .
	docker build -t svetlyak40wt/sovcomtrade-2022-app:latest -f docker/Dockerfile --build-arg APP=app .
