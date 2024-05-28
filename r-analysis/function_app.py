import azure.functions as func
import logging
import subprocess

app = func.FunctionApp(http_auth_level=func.AuthLevel.FUNCTION)

@app.route(route="physical_match_form")
def physical_match_form(req: func.HttpRequest) -> func.HttpResponse:
    logging.info('Python HTTP trigger function processed a request.')

    # name = req.params.get('name')
    # if not name:
    #     try:
    #         req_body = req.get_json()
    #     except ValueError:
    #         pass
    #     else:
    #         name = req_body.get('name')

    # if name:
    #     return func.HttpResponse(f"Hello, {name}. This HTTP triggered function executed successfully.")
    # else:
    #     return func.HttpResponse(
    #          "This HTTP triggered function executed successfully. Pass a name in the query string or in the request body for a personalized response.",
    #          status_code=200
    #     )
    logging.info('Python HTTP trigger function processed a request.')

    # Call the R script
    result = subprocess.run(['Rscript', '/physical_analysis/API_physical_match_form_FEY.R'], capture_output=True, text=True)

    # Check if the R script execution was successful
    if result.returncode != 0:
        logging.error(f"R script error: {result.stderr}")
        return func.HttpResponse(f"R script execution failed: {result.stderr}", status_code=500)

    logging.info(f"R script output: {result.stdout}")
    return func.HttpResponse(f"R script executed successfully: {result.stdout}", status_code=200)