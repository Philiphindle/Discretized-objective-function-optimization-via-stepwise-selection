"""
Creates an API for requests to the DDS Algorithm, using fastapi
Makes use of pydantic for data validation

For testing, navigate to the git directory, type uvicorn api.main:app -- reload
and go to http://127.0.0.1:8000/
Since we used --reload for development, when we update our application code, the server will
automatically reload

see: fastapi.tiangolo.com
"""

import pandas as pd
import numpy as np
from fastapi import FastAPI
import uvicorn # server for running the api- serves in our local machine
from pydantic import BaseModel

# FastAPI is a class that provides all the functionality for our api
# Create an instance of FastAPI
app = FastAPI()

# Get is an operation, used to reference one of the HTTP request methods:

# POST
# GET
# PUT
# DELETE
# OPTIONS
# HEAD

# Define path operator decorator- the function below handles requests that go to the 
# path "/" using a get operation (requests.get, etc.)
@app.get("/")
async def root():
    # return a dictionary/json file
    return {"message": "This is the DDS Algorithm"}

# we can go to http://127.0.0.1:8000/stocks/microsoft
# setting str enables data validation- if a value that can't be coerced to a string is 
# input, an exception will be raised (effectively using pydantic, since this is more
# than just a type hint)
@app.get("/stocks/{stock_id}")
async def read_stock(stock_id: str):
    # here we would place our function, or a call to a function in that package- could place
    # as a docker image on AWS Lambda
    return {"message": stock_id}