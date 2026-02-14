// 🐿️ HASEL — Haskell-Style Monads for Eichhörnchen
// The philosophical heart of the Master MCP Server

import type { MonadType, ToolResult } from './types.js';

/**
 * Base Monad class — the Nuss of all computations
 */
export abstract class Monad<T> {
  constructor(protected value: T) {}

  /**
   * fmap — Transform the value inside the monad
   */
  abstract map<U>(fn: (value: T) => U): Monad<U>;

  /**
   * bind (>>=) — Chain monadic computations
   */
  abstract flatMap<U>(fn: (value: T) => Monad<U>): Monad<U>;

  /**
   * Extract the value (use with care!)
   */
  getValue(): T {
    return this.value;
  }

  /**
   * Get the monad type
   */
  abstract getMonadType(): MonadType;
}

/**
 * TextMonad — Pure text transformations
 */
export class TextMonad extends Monad<string> {
  map<U>(fn: (value: string) => U): Monad<U> {
    if (typeof fn(this.value) === 'string') {
      return new TextMonad(fn(this.value) as string) as unknown as Monad<U>;
    }
    return new IOMonad(fn(this.value)) as Monad<U>;
  }

  flatMap<U>(fn: (value: string) => Monad<U>): Monad<U> {
    return fn(this.value);
  }

  getMonadType(): MonadType {
    return 'text';
  }

  // Helper: Convert to uppercase
  toUpper(): TextMonad {
    return new TextMonad(this.value.toUpperCase());
  }

  // Helper: Convert to lowercase
  toLower(): TextMonad {
    return new TextMonad(this.value.toLowerCase());
  }

  // Helper: Trim whitespace
  trim(): TextMonad {
    return new TextMonad(this.value.trim());
  }
}

/**
 * BinaryMonad — Binary data (buffers, images, etc.)
 */
export class BinaryMonad extends Monad<Buffer> {
  map<U>(fn: (value: Buffer) => U): Monad<U> {
    const result = fn(this.value);
    if (Buffer.isBuffer(result)) {
      return new BinaryMonad(result) as unknown as Monad<U>;
    }
    return new IOMonad(result) as Monad<U>;
  }

  flatMap<U>(fn: (value: Buffer) => Monad<U>): Monad<U> {
    return fn(this.value);
  }

  getMonadType(): MonadType {
    return 'binary';
  }

  // Helper: Get size in bytes
  size(): number {
    return this.value.length;
  }

  // Helper: Convert to base64
  toBase64(): TextMonad {
    return new TextMonad(this.value.toString('base64'));
  }
}

/**
 * URLMonad — Web resources
 */
export class URLMonad extends Monad<string> {
  map<U>(fn: (value: string) => U): Monad<U> {
    const result = fn(this.value);
    if (typeof result === 'string') {
      return new URLMonad(result) as unknown as Monad<U>;
    }
    return new IOMonad(result) as Monad<U>;
  }

  flatMap<U>(fn: (value: string) => Monad<U>): Monad<U> {
    return fn(this.value);
  }

  getMonadType(): MonadType {
    return 'url';
  }

  // Helper: Validate URL
  isValid(): boolean {
    try {
      new URL(this.value);
      return true;
    } catch {
      return false;
    }
  }

  // Helper: Get domain
  getDomain(): string | null {
    try {
      return new URL(this.value).hostname;
    } catch {
      return null;
    }
  }
}

/**
 * IPFSMonad — IPFS distributed content
 */
export class IPFSMonad extends Monad<string> {
  map<U>(fn: (value: string) => U): Monad<U> {
    const result = fn(this.value);
    if (typeof result === 'string' && this.isValidCID(result)) {
      return new IPFSMonad(result) as unknown as Monad<U>;
    }
    return new IOMonad(result) as Monad<U>;
  }

  flatMap<U>(fn: (value: string) => Monad<U>): Monad<U> {
    return fn(this.value);
  }

  getMonadType(): MonadType {
    return 'ipfs';
  }

  // Helper: Validate CID format
  private isValidCID(cid: string): boolean {
    return /^Qm[1-9A-HJ-NP-Za-km-z]{44,}$|^b[A-Za-z2-7]{58,}$|^[Ff][0-9A-Fa-f]{50,}$/.test(cid);
  }

  // Helper: Check if valid CID
  isValid(): boolean {
    return this.isValidCID(this.value);
  }

  // Helper: Get gateway URL
  toGatewayURL(gateway: string = 'https://ipfs.io'): string {
    return `${gateway}/ipfs/${this.value}`;
  }
}

/**
 * DOMMonad — Browser DOM operations
 */
export class DOMMonad extends Monad<unknown> {
  map<U>(fn: (value: unknown) => U): Monad<U> {
    return new DOMMonad(fn(this.value)) as Monad<U>;
  }

  flatMap<U>(fn: (value: unknown) => Monad<U>): Monad<U> {
    return fn(this.value);
  }

  getMonadType(): MonadType {
    return 'dom';
  }
}

/**
 * DNSMonad — DNS operations
 */
export class DNSMonad extends Monad<string> {
  map<U>(fn: (value: string) => U): Monad<U> {
    const result = fn(this.value);
    if (typeof result === 'string') {
      return new DNSMonad(result) as unknown as Monad<U>;
    }
    return new IOMonad(result) as Monad<U>;
  }

  flatMap<U>(fn: (value: string) => Monad<U>): Monad<U> {
    return fn(this.value);
  }

  getMonadType(): MonadType {
    return 'dns';
  }

  // Helper: Validate hostname
  isValidHostname(): boolean {
    return /^([a-z0-9]+(-[a-z0-9]+)*\.)+[a-z]{2,}$/i.test(this.value);
  }
}

/**
 * ClipboardMonad — Clipboard operations
 */
export class ClipboardMonad extends Monad<string> {
  map<U>(fn: (value: string) => U): Monad<U> {
    const result = fn(this.value);
    if (typeof result === 'string') {
      return new ClipboardMonad(result) as unknown as Monad<U>;
    }
    return new IOMonad(result) as Monad<U>;
  }

  flatMap<U>(fn: (value: string) => Monad<U>): Monad<U> {
    return fn(this.value);
  }

  getMonadType(): MonadType {
    return 'clipboard';
  }
}

/**
 * IOMonad — General I/O operations (the universal monad)
 */
export class IOMonad<T = unknown> extends Monad<T> {
  map<U>(fn: (value: T) => U): Monad<U> {
    return new IOMonad(fn(this.value));
  }

  flatMap<U>(fn: (value: T) => Monad<U>): Monad<U> {
    return fn(this.value);
  }

  getMonadType(): MonadType {
    return 'io';
  }
}

/**
 * MixedMonad — Combined text + binary
 */
export class MixedMonad extends Monad<{ text: string; binary: Buffer }> {
  map<U>(fn: (value: { text: string; binary: Buffer }) => U): Monad<U> {
    return new IOMonad(fn(this.value)) as Monad<U>;
  }

  flatMap<U>(fn: (value: { text: string; binary: Buffer }) => Monad<U>): Monad<U> {
    return fn(this.value);
  }

  getMonadType(): MonadType {
    return 'mixed';
  }
}

/**
 * Factory function to create monads from values
 */
export function createMonad(type: MonadType, value: unknown): Monad<unknown> {
  switch (type) {
    case 'text':
      return new TextMonad(String(value));
    case 'binary':
      return new BinaryMonad(Buffer.from(value as ArrayBuffer));
    case 'url':
      return new URLMonad(String(value));
    case 'ipfs':
      return new IPFSMonad(String(value));
    case 'dom':
      return new DOMMonad(value);
    case 'dns':
      return new DNSMonad(String(value));
    case 'clipboard':
      return new ClipboardMonad(String(value));
    case 'mixed':
      return new MixedMonad(value as { text: string; binary: Buffer });
    case 'io':
    default:
      return new IOMonad(value);
  }
}

/**
 * Result monad for operations that can fail
 */
export class Result<T, E = Error> {
  private constructor(
    private readonly success: boolean,
    private readonly value?: T,
    private readonly error?: E
  ) {}

  static ok<T>(value: T): Result<T, never> {
    return new Result<T, never>(true, value, undefined);
  }

  static err<E>(error: E): Result<never, E> {
    return new Result<never, E>(false, undefined, error);
  }

  isOk(): this is Result<T, never> {
    return this.success;
  }

  isErr(): this is Result<never, E> {
    return !this.success;
  }

  unwrap(): T {
    if (!this.success) {
      throw new Error('Called unwrap on an Err value');
    }
    return this.value!;
  }

  unwrapOr(defaultValue: T): T {
    return this.success ? this.value! : defaultValue;
  }

  map<U>(fn: (value: T) => U): Result<U, E> {
    if (!this.success) {
      return Result.err(this.error!);
    }
    return Result.ok(fn(this.value!));
  }

  mapErr<F>(fn: (error: E) => F): Result<T, F> {
    if (this.success) {
      return Result.ok(this.value!);
    }
    return Result.err(fn(this.error!));
  }

  toToolResult(): ToolResult {
    return {
      success: this.success,
      data: this.value,
      error: this.error ? String(this.error) : undefined,
    };
  }
}
