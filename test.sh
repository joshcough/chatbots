#!/usr/bin/env bash

stack exec weeder -- . --build
stack run formatter

export ROLLBAR_TOKEN="undefined"
export ROLLBAR_ENVIRONMENT="undefined"
export CHATBOT_NICK="undefined"
export CHATBOT_PASS="undefined"
export ROLLBAR_TOKEN="undefined"
stack test
