import { app, HttpRequest, HttpResponseInit, InvocationContext } from '@azure/functions';
import { exec } from 'child_process';
import * as util from 'util'

export async function firstContactCorner(request: HttpRequest, context: InvocationContext): Promise<HttpResponseInit> {
    context.log(`Http function processed request for url "${request.url}"`);

    const execAsync = util.promisify(exec);

    try {
        const result = await execAsync('Rscript physical_analysis/API_physical_match_form_FEY.R')
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

app.http('firstContactCorner', {
    methods: ['GET', 'POST'],
    authLevel: 'anonymous',
    handler: firstContactCorner
});
