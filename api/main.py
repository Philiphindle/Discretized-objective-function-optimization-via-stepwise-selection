"""
Creates an API for requests to the DDS Algorithm, using fastapi
Makes use of pydantic for data validation


see: fastapi.tiangolo.com
"""

import pandas as pd
import numpy as np
from fastapi import FastAPI
import uvicorn # server for running the api- serves in our local machine
from pydantic import BaseModel

# FastAPI is a class that provides all the functionality for our api
app = FastAPI()

@app.get("/")
async def root():
    return("This is the DDS Algorithm")
