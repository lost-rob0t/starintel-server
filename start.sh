docker run -d -p 5672:5672 -p 15672:15672 -e RABBITMQ_USER=user -e RABBITMQ_PASS=password   rabbitmq:3.13.1-management
