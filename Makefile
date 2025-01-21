##
## EPITECH PROJECT, 2024
## B-FUN-500-STG-5-2-glados-augustin.grosnon
## File description:
## Makefile
##

BINARY_PATH	:=	$(shell stack --allow-different-user path --local-install-root)

NAME	=	glados

all: $(NAME) cp_binary

$(NAME):
	@echo "Building..."
	@stack --allow-different-user build --verbosity warn

cp_binary:
	@cp $(BINARY_PATH)/bin/$(NAME)-exe ./$(NAME)
	@echo "Copied $(NAME) binary to root!"

clean:
	@echo "Cleaning..."
	@stack --allow-different-user clean

fclean:	clean
	@echo "Executing a full clean..."
	@rm -f $(NAME)
	@rm -rf .stack-work/
	@rm -f $(NAME).cabal
	@rm -f stack.yaml.lock
	@rm -f coding-style-reports.log

tests_run:
	@echo "Running tests"
	@stack --allow-different-user test --coverage --verbosity warn

style_check: fclean
	@echo "Running coding-style"
	@coding-style . .
	@cat coding-style-reports.log

re:	fclean all

.PHONY:	all cp_binary clean fclean re tests_run style_check
