/**
 * 🐿️ HASEL Monads — The Philosophical Heart of Squirrel OS
 *
 * HASEL = Haskell + Squirrel + Monad
 * Every Nuss (data) flows through a Monad, ensuring type safety and composability
 */

// ═══════════════════════════════════════════════════
// 🌰 Core Monad Type
// ═══════════════════════════════════════════════════

export type MonadType =
  | 'text'      // Pure text data (strings)
  | 'binary'    // Binary data (buffers, images)
  | 'mixed'     // Mixed content (JSON with embedded binary)
  | 'url'       // Web resources
  | 'ipfs'      // IPFS content
  | 'dom'       // DOM/HTML structures
  | 'dns'       // DNS records
  | 'clipboard' // Clipboard data
  | 'io'        // I/O operations
  | 'error';    // Error state

export interface Nuss<T = unknown> {
  type: MonadType;
  value: T;
  metadata?: Record<string, unknown>;
  timestamp: number;
}

// ═══════════════════════════════════════════════════
// 🐿️ Monad Factory — Creating Nüsse
// ═══════════════════════════════════════════════════

export function createNuss<T>(type: MonadType, value: T, metadata?: Record<string, unknown>): Nuss<T> {
  return {
    type,
    value,
    metadata: metadata || {},
    timestamp: Date.now(),
  };
}

// ═══════════════════════════════════════════════════
// 🌲 Monad Operations — The NussKette (Nut Chain)
// ═══════════════════════════════════════════════════

export class NussMonad<T> {
  constructor(private nuss: Nuss<T>) {}

  // Map: Transform the Nuss value
  map<U>(fn: (value: T) => U): NussMonad<U> {
    try {
      const newValue = fn(this.nuss.value);
      return new NussMonad(createNuss(this.nuss.type, newValue, this.nuss.metadata));
    } catch (error) {
      return new NussMonad(createNuss('error', error, { originalType: this.nuss.type })) as any as NussMonad<U>;
    }
  }

  // FlatMap: Chain operations that return Monads
  flatMap<U>(fn: (value: T) => NussMonad<U>): NussMonad<U> {
    try {
      if (this.nuss.type === 'error') {
        return this as any as NussMonad<U>;
      }
      return fn(this.nuss.value);
    } catch (error) {
      return new NussMonad(createNuss('error', error, { originalType: this.nuss.type })) as any as NussMonad<U>;
    }
  }

  // Filter: Conditional processing
  filter(predicate: (value: T) => boolean): NussMonad<T | null> {
    if (this.nuss.type === 'error') {
      return this as any as NussMonad<T | null>;
    }
    const passes = predicate(this.nuss.value);
    return passes
      ? this as any as NussMonad<T | null>
      : new NussMonad(createNuss(this.nuss.type, null as T | null, this.nuss.metadata));
  }

  // Unwrap: Extract the value (unsafe!)
  unwrap(): T {
    if (this.nuss.type === 'error') {
      throw this.nuss.value;
    }
    return this.nuss.value;
  }

  // Safe unwrap with default
  unwrapOr(defaultValue: T): T {
    if (this.nuss.type === 'error') {
      return defaultValue;
    }
    return this.nuss.value;
  }

  // Get the full Nuss
  getNuss(): Nuss<T> {
    return this.nuss;
  }

  // Check if error
  isError(): boolean {
    return this.nuss.type === 'error';
  }
}

// ═══════════════════════════════════════════════════
// 🔗 NussKette Pipeline — Composing Operations
// ═══════════════════════════════════════════════════

export type PipelineStep<T, U> = (monad: NussMonad<T>) => NussMonad<U> | Promise<NussMonad<U>>;

export class NussKette<T> {
  private steps: PipelineStep<any, any>[] = [];

  constructor(private initialMonad: NussMonad<T>) {}

  // Add a step to the pipeline
  pipe<U>(step: PipelineStep<any, U>): NussKette<U> {
    this.steps.push(step);
    return this as unknown as NussKette<U>;
  }

  // Execute the pipeline
  async execute(): Promise<NussMonad<any>> {
    let current: NussMonad<any> = this.initialMonad;

    for (const step of this.steps) {
      if (current.isError()) {
        break; // Stop on error
      }
      current = await step(current);
    }

    return current;
  }

  // Execute and unwrap
  async run(): Promise<any> {
    const result = await this.execute();
    return result.unwrap();
  }

  // Execute with error handling
  async runSafe(defaultValue: any): Promise<any> {
    try {
      const result = await this.execute();
      return result.unwrapOr(defaultValue);
    } catch (error) {
      return defaultValue;
    }
  }
}

// ═══════════════════════════════════════════════════
// 🎯 Monad Constructors — Sugar Syntax
// ═══════════════════════════════════════════════════

export const Monads = {
  text: (value: string, metadata?: Record<string, unknown>) =>
    new NussMonad(createNuss('text', value, metadata)),

  binary: (value: Buffer, metadata?: Record<string, unknown>) =>
    new NussMonad(createNuss('binary', value, metadata)),

  mixed: (value: any, metadata?: Record<string, unknown>) =>
    new NussMonad(createNuss('mixed', value, metadata)),

  url: (value: string, metadata?: Record<string, unknown>) =>
    new NussMonad(createNuss('url', value, metadata)),

  ipfs: (value: string, metadata?: Record<string, unknown>) =>
    new NussMonad(createNuss('ipfs', value, metadata)),

  dom: (value: any, metadata?: Record<string, unknown>) =>
    new NussMonad(createNuss('dom', value, metadata)),

  dns: (value: any, metadata?: Record<string, unknown>) =>
    new NussMonad(createNuss('dns', value, metadata)),

  clipboard: (value: string, metadata?: Record<string, unknown>) =>
    new NussMonad(createNuss('clipboard', value, metadata)),

  io: (value: any, metadata?: Record<string, unknown>) =>
    new NussMonad(createNuss('io', value, metadata)),

  error: (error: Error | unknown, metadata?: Record<string, unknown>) =>
    new NussMonad(createNuss('error', error, metadata)),
};

// ═══════════════════════════════════════════════════
// 🔄 Async Monad Helpers
// ═══════════════════════════════════════════════════

export async function liftAsync<T, U>(
  monad: NussMonad<T>,
  asyncFn: (value: T) => Promise<U>
): Promise<NussMonad<U>> {
  try {
    if (monad.isError()) {
      return monad as any as NussMonad<U>;
    }
    const result = await asyncFn(monad.unwrap());
    const nuss = monad.getNuss();
    return new NussMonad(createNuss(nuss.type, result, nuss.metadata));
  } catch (error) {
    return Monads.error(error) as any as NussMonad<U>;
  }
}

// Sequence: Run monads in parallel and collect results
export async function sequence<T>(monads: NussMonad<T>[]): Promise<NussMonad<T[]>> {
  try {
    const values = monads.map(m => m.unwrap());
    return Monads.mixed(values) as any as NussMonad<T[]>;
  } catch (error) {
    return Monads.error(error) as any as NussMonad<T[]>;
  }
}

// ═══════════════════════════════════════════════════
// 🎨 Pretty Printing
// ═══════════════════════════════════════════════════

export function formatNuss(nuss: Nuss<unknown>): string {
  const emoji = {
    text: '📝',
    binary: '🔲',
    mixed: '🎨',
    url: '🌐',
    ipfs: '🪐',
    dom: '🎭',
    dns: '🏠',
    clipboard: '📋',
    io: '💾',
    error: '❌',
  }[nuss.type];

  return `${emoji} [${nuss.type}] ${JSON.stringify(nuss.value, null, 2)}`;
}
