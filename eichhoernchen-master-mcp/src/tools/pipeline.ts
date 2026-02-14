// 🐿️ Pipeline Tools — NussKette (Nut Chain) Pipelines

import type { ToolResult, ResponseFormat, PipelineStep } from '../types.js';
import { formatResponse, successResult, errorResult } from '../shared.js';
import { createMonad } from '../hasel.js';

export interface NussKetteParams {
  name: string;
  description?: string;
  steps: PipelineStep[];
  response_format: ResponseFormat;
}

/**
 * Execute a NussKette pipeline
 * This chains multiple tools together, passing output from one to the next
 */
export async function nusskette(params: NussKetteParams, toolExecutor: ToolExecutor): Promise<ToolResult> {
  try {
    const results: Array<{
      step: number;
      monad: string;
      action: string;
      status: string;
      result?: unknown;
      error?: string;
    }> = [];

    let currentValue: unknown = null;

    for (let i = 0; i < params.steps.length; i++) {
      const step = params.steps[i];

      try {
        // Create monad for this step
        const monad = createMonad(step.monad, currentValue || step.params);

        // Execute the tool
        const toolResult = await toolExecutor.executeTool(step.action, {
          ...step.params,
          // If there's a current value, try to inject it
          ...(currentValue && typeof currentValue === 'object' ? currentValue : {}),
        });

        if (toolResult.success) {
          currentValue = toolResult.data;
          results.push({
            step: i + 1,
            monad: step.monad,
            action: step.action,
            status: 'success',
            result: toolResult.data,
          });
        } else {
          results.push({
            step: i + 1,
            monad: step.monad,
            action: step.action,
            status: 'error',
            error: toolResult.error,
          });
          // Stop pipeline on error
          break;
        }
      } catch (error) {
        results.push({
          step: i + 1,
          monad: step.monad,
          action: step.action,
          status: 'error',
          error: error instanceof Error ? error.message : String(error),
        });
        // Stop pipeline on error
        break;
      }
    }

    const result = {
      pipeline: params.name,
      description: params.description,
      totalSteps: params.steps.length,
      completedSteps: results.filter((r) => r.status === 'success').length,
      results,
      finalOutput: currentValue,
    };

    return successResult(formatResponse(result, params.response_format), {
      completedSteps: results.filter((r) => r.status === 'success').length,
      totalSteps: params.steps.length,
    });
  } catch (error) {
    return errorResult(error as Error);
  }
}

/**
 * Tool executor interface for pipeline
 */
export interface ToolExecutor {
  executeTool(name: string, params: unknown): Promise<ToolResult>;
}
