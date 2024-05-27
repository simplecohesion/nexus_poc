import { app, HttpRequest, HttpResponseInit, InvocationContext } from "@azure/functions";
import { exec } from 'child_process';
import * as util from 'util'

export async function physicalMatchForm(request: HttpRequest, context: InvocationContext): Promise<HttpResponseInit> {
  context.log(`Http function processed request for url "${request.url}"`);

  // const name = request.query.get('name') || await request.text() || 'world';

  // return { body: `Hello, ${name}!` };

  const execAsync = util.promisify(require('child_process').exec);

  try {
    const result = await execAsync('Rscript /r-script/physical_analysis/API_physical_match_form_FEY.R')
    return {
      status: 200,
      body: `R script executed successfully: ${result}`
    };
  }
  catch (e) {
    return {
      status: 500,
      body: `R script execution failed: ${e}`
    };
  }
};

app.http('physicalMatchForm', {
  methods: ['GET', 'POST'],
  authLevel: 'anonymous',
  handler: physicalMatchForm
});
