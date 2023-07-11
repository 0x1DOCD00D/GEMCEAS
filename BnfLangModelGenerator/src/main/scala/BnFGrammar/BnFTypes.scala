/*******************************************************************************
 *
 *  Copyright (c) 7/11/23, 12:59 PM by Mark Grechanik (drmark) and Lone Star Consulting, Inc. All rights reserved.
 *  
 *  Licensed under the Apache License, Version 2.0 (the “License”); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0
 *  
 *  Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an “AS IS” BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 *  
 ******************************************************************************/

package BnFGrammar

/*
 mainRule           ::= rule | rule mainRule
 rule               ::= <rule-name> "::=" expression
 <rule-name>        ::= "[<a-zA-Z][-#>$\.:_a-zA-Z0-9]*"
 expression         ::= list | list "|" expression | "[" expression "]" | "{" expression "}"
 list               ::= term | term list
 term               ::= <literal> | <rule-name>
 <literal>          ::= "\".*\"" | "\'\.*\'"
 */

enum BnFTypes:
  case MainRule(rule: BnFTypes)