# CMPT 436
# Assignment 3
# Vitalii Radzividlo
# vir138
# 11247071

FROM erlang as Server
EXPOSE 8080

WORKDIR /usr/src/app

# COPY server.erl .
CMD [ "/bin/bash" ]


FROM erlang as Client
EXPOSE 8080

WORKDIR /usr/src/app

# COPY client.erl .
CMD ["/bin/bash"]




