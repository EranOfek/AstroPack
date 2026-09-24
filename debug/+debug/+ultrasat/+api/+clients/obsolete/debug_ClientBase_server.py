from fastapi import FastAPI, Request
from pydantic import BaseModel
import json


app = FastAPI()

class MathRequest(BaseModel):
    a: float
    b: float

class MathResponse(BaseModel):
    result: float
    status: str
    message: str


@app.post("/add")
async def add_numbers(request: Request):
    try:
        body = await request.json()  # Read raw JSON from MATLAB
        print(f"Received JSON from MATLAB: {json.dumps(body, indent=2)}")  # Pretty print request

        if not isinstance(body, dict) or "a" not in body or "b" not in body:
            return {"error": "Invalid JSON structure received"}

        # Convert to float (in case MATLAB sends strings)
        a = float(body["a"])
        b = float(body["b"])

        return {
            "result": a + b,
            "status": "success",
            "message": f"Addition: {a} + {b} = {a + b}"
        }
    except Exception as e:
        print(f"Error: {str(e)}")
        return {"error": str(e)}


"""
@app.post("/add")
async def add_numbers(req: MathRequest):
    return MathResponse(
        result=req.a + req.b,
        status="success",
        message=f"Addition: {req.a} + {req.b} = {req.a + req.b}"
    )
"""


@app.post("/multiply", response_model=MathResponse)
async def multiply_numbers(req: MathRequest):
    return MathResponse(
        result=req.a * req.b,
        status="success",
        message=f"Multiplication: {req.a} * {req.b} = {req.a * req.b}"
    )

if __name__ == "__main__":
    import uvicorn
    uvicorn.run(app, host="127.0.0.1", port=8299)
