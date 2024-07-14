FROM r-base:4.4.1

RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev

COPY dependencies.R .
RUN Rscript dependencies.R
COPY . .

EXPOSE 4000
CMD ["Rscript", "app.R"]
