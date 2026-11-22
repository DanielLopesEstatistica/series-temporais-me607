# Credenciais — e-mail (Trabalho Final)

## Opção A — JSON do Google (recomendado)

1. [Google Cloud Console — Credenciais](https://console.cloud.google.com/apis/credentials)
2. Crie credenciais **OAuth 2.0** (tipo Desktop ou Web).
3. Baixe o JSON e salve como `secrets/google-oauth-client.json` (ou use o arquivo movido da raiz do projeto).
4. Ative a [Gmail API](https://console.cloud.google.com/apis/library/gmail.googleapis.com).

## Opção B — variáveis de ambiente

Copie `.Renviron.example` para `secrets/.Renviron` (mesmas chaves que no app do Ponto Eletrônico).

## Referências

- [gmailr — OAuth from a JSON file](https://gmail.r-lib.org/reference/gm_auth_configure.html)
- [gargle — API credentials](https://gargle.r-lib.org/articles/get-api-credentials.html)