---
title: "A typescript client for an ADL API"
author: Tim Docker
date: 2020-05-22
tags: [programming]
---

# Introduction

This is the third post in a series where we use [ADL][] to build a
multi-language system with consistent types. Previously, we have

* [defined the API in ADL][post1]
* [implemented a server for the API in haskell][post2]

Here we will implement a statically typed client for the API in
typescript.

I think that typescript is presently a sweet spot
for web development: it has a decent static type system;
it integrates trivially with the rest of the javascript ecosystem; and
it has achieved mainstream acceptance. Using ADL to ensure consistent
types between the server and a typescript web application greatly boosts
developer productivity, especially over time as the API grows.

[ADL]:https://github.com/timbod7/adl
[post1]:/posts/2020-04-24-adl-example-api.html
[post2]:/posts/2020-05-01-adl-example-haskell.html
[api-def]:https://github.com/timbod7/adl-demo/blob/master/messageboard-api/adl/api.adl

# Our tools

In this post we will focus on a client library for the API, so our external
dependencies will be limited. Later, we will create a full web application.

We'll keep our code small and leverage the typescript ecosystem, making
use of just a [few external dependencies][packagejson]. At runtime:

* [node-fetch][] is used to so we can make API calls from the node VM (as well as the browser).
* [base64-js][] is required by the ADL typescript runtime

[packagejson]:https://github.com/timbod7/adl-demo/tree/master/messageboard-api/typescript/package.json
[node-fetch]:https://www.npmjs.com/package/node-fetch
[base64-js]:https://www.npmjs.com/package/base64-js

# The code structure

For reference, the project code structure is as below. There are also the usual files
to support typescript and yarn/npm.

| File                                                                        | Description                         |
| :-                                                                          | :-                                  |
| [`messageboard-api/adl/*`][mbadl]                                           | the ADL definitions                 |
| [`messageboard-api/scripts/generate-adl.sh`][generate-adl.sh]               | script to generate code from ADL    |
| [`messageboard-api/typescript/src/adl/*`][mbadlts]                          | typescript code generated from the ADL | 
| [`messageboard-api/typescript/src/service/service.ts`][service.ts]          | The service implementation | 
| [`messageboard-api/typescript/src/service/http.ts`][http.ts]                | Abstraction for http communications | 

[mbadl]:https://github.com/timbod7/adl-demo/tree/master/messageboard-api/adl
[mbadlts]:https://github.com/timbod7/adl-demo/tree/master/messageboard-api/typescript/src/adl
[generate-adl.sh]:https://github.com/timbod7/adl-demo/blob/master/messageboard-api/scripts/generate-adl.sh
[service.ts]:https://github.com/timbod7/adl-demo/blob/master/messageboard-api/typescript/src/service/service.ts
[http.ts]:https://github.com/timbod7/adl-demo/blob/master/messageboard-api/typescript/src/service/http.ts

# An http abstraction

We want to be able to make typed API requests from both the browser, and also from a nodejs VM.
However the underlying machinery for making http requests differs in those environments. Hence we
will build our typed API atop a trivial [http abstraction][http.ts]:

```
export interface HttpFetch {
  fetch(request: HttpRequest): Promise<HttpResponse>;
}

export interface HttpHeaders {
  [index: string]: string;
}

export interface HttpRequest {
  url: string;
  headers: HttpHeaders;
  method: "get" | "put" | "post";
  body?: string;
}

export interface HttpResponse {
  status: number;
  statusText: string;
  ok: boolean;
  text(): Promise<string>;
  json(): Promise<{} | null>;
}
```

The two implementations of this are [node-http.ts][] and [browser-http.ts][]. 

[node-http.ts]:https://github.com/timbod7/adl-demo/blob/master/messageboard-api/typescript/src/service/node-http.ts
[browser-http.ts]:https://github.com/timbod7/adl-demo/blob/master/messageboard-api/typescript/src/service/browser-http.ts

# Request types and the Api interface

Referring back to the [original API design][post1], there are two distinct types of requests:
those that are public and don't require an auth token, and the authenticated
requests that do. A public request in the ADL of type [`HttpPost<I,O>`][adlhttppost]
will be mapped to a `ReqFn<I,O>` in typescript:

```
export type ReqFn<I, O> = (req: I) => Promise<O>;
```

whereas an authenticated request in the ADL of type [`HttpPost<I,O>`][adlhttppost] will be mapped
to an `AuthReqFn<I,O>` in typescript:

```
export type AuthReqFn<I, O> = (authToken: string, req: I) => Promise<O>;
```

[adlhttppost]:https://github.com/timbod7/adl-demo/blob/master/messageboard-api/adl/types.adl#L17

Given these types, our service matching the ADL type `Api` will meet this typescript
interface:

```
import * as API from "../adl/api";

interface Api {
  login: ReqFn<API.LoginReq, API.LoginResp>;
  ping: ReqFn<Empty, Empty>;
  newMessage: AuthReqFn<API.NewMessageReq,Empty>;
  recentMessages: AuthReqFn<API.RecentMessagesReq,API.Message[]>;
  createUser: AuthReqFn<API.CreateUserReq,API.CreateUserResp>;
};
```

The typescript code generated from the ADL contains sufficient metadata
to be able to derive both the `ReqFn<>` or `AuthReqFn<>` without hand
written code. As a concrete example, consider the the `recentMessages`
ADL endpoint definition:

```
  HttpPost<RecentMessagesReq,Vector<Message>> recentMessages = {
    "path" : "/recent-messages",
    "security" : "token"
  };
```  

The typescript function that implements this will have type
`AuthReqFn<API.RecentMessagesReq,API.Message[]>` and needs to:

* Serialise the value of type `RecentMessagesReq` to json
* Make an http post request to the `/recent-messages` path, with the json body
  and the provided auth token in the `Authorization` header.
* Wait for the response
* Deserialise the json response to a value of type `Message[]` and
  return as the result of the promise.

We need equivalent logic for every authenticated request. The public
requests are almost the same, leaving out the auth token and  header.

In our typescript API client, we put the code for this abstracted request
logic in the `ServiceBase` class:

```
import { HttpFetch, HttpRequest } from "./http";
import * as ADL from "../adl/runtime/adl";
import { HttpPost } from "../adl/types";

export class ServiceBase {
  
   constructor(
    private readonly http: HttpFetch,
    private readonly baseUrl: string,
    private readonly resolver: ADL.DeclResolver,
  ) {
  }
  
  mkPostFn<I, O>(rtype: HttpPost<I, O>): ReqFn<I, O> {...}

  mkAuthPostFn<I, O>(rtype: HttpPost<I, O>): AuthReqFn<I, O> {...}
};
```

This class constructor needs the request abstraction `http`, the `baseUrl` to
which requests will be made, and also the ADL `resolver`. A `DeclResolver`
provides access to metadata for all ADL declarations. The class provides
two member functions for constructing `ReqFn` or `AuthReqFn` values
from ADL API endpoint definitions. The [implementation][service-base.ts]
of these two functions is straightforward.

[service-base.ts]:https://github.com/timbod7/adl-demo/blob/master/messageboard-api/typescript/src/service/service-base.ts


# The implementation

Given these functions in the `ServiceBase` class, the implementation of
our client is straightforward. The entire code is:


```
import { HttpFetch } from "./http";
import * as ADL from "../adl/runtime/adl";
import * as API from "../adl/api";
import { AuthReqFn, ReqFn, ServiceBase } from "./service-base";
import { Jwt, Empty } from "../adl/types";

const api = API.makeApi({});

// Implements typed access to the authenticated API endpoints
export class Service extends ServiceBase {
  constructor(
    http: HttpFetch,
    baseUrl: string,
    resolver: ADL.DeclResolver,
  ) {
    super(http, baseUrl, resolver);
  }

  login: ReqFn<API.LoginReq, API.LoginResp> = this.mkPostFn(api.login);
  ping: ReqFn<Empty, Empty> = this.mkPostFn(api.ping);
  newMessage: AuthReqFn<API.NewMessageReq,Empty> = this.mkAuthPostFn(api.newMessage);
  recentMessages: AuthReqFn<API.RecentMessagesReq,API.Message[]> = this.mkAuthPostFn(api.recentMessages);
  createUser: AuthReqFn<API.CreateUserReq,API.CreateUserResp> = this.mkAuthPostFn(api.createUser);
};
```

If a new endpoint is added to the API, then just a single line needs to be added to
this implementation. And the end to end usage of ADL ensures that all of the types
are consistent and compile time checked, from the server through to the client.

# Testing

First start the server, as per the [previous post][post1]:

```
$ cd messageboard-api/haskell
$ stack run messageboard-server server-config.yaml

spock is running on port 8080
```

Then we can write a simple typescript script to exercise our API from
nodejs:

After some imports:

```
import {Service} from './service/service';
import {NodeHttp} from './service/node-http';
import {RESOLVER} from './adl/resolver';
import * as API from "./adl/api";
```

we instantiate the service client:

```
  const http = new NodeHttp();
  const service = new Service(http, "http://localhost:8080", RESOLVER);
```

and call the public ping endpoint:

```
  await service.ping({});
```

Logging in is also a public method, but on success returns a token so
that we can subsequently call authenticated methods:

```
  const resp = await service.login({
      email: "admin@test.com",
      password: "xyzzy",
    });
  assert(resp.kind == 'success');
  const adminToken = resp.value;
```

Hence, as admin, we can post some messages:

```
  await service.newMessage(adminToken, {body: "Hello message board!"});
  await service.newMessage(adminToken, {body: "It's quiet around here!"});```
```

The [`service-tests.ts`][service-tests.ts] script exercises the API more
fully. You can run it directly using the `ts-node` command.

[service-tests.ts]:https://github.com/timbod7/adl-demo/blob/master/messageboard-api/typescript/src/service-tests.ts

# Summing Up

We now have end-to-end type safety between our server and our client, despite the fact
they are written in different languages. This is a big step forward in developer
productivity. For example, one can extend or refactor the API using the
same approach one would in any strongly statically typed environment: change it, and then
be guided by the compiler errors to find and fix affected server and browser code.

I'm unsure what posts will follow in this series... I may look at:

  * implementing the server in rust or typescript
  * using ADL to define a persistence layer behind the server
  * using this typescript API client in a react application

Feel free to post questions and comments as issues on the [project repo][adldemo].

[adldemo]:https://github.com/timbod7/adl-demo

