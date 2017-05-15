(* Copyright (C) 2013  Petter Urkedal <paurkedal@gmail.com>
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version, with the OCaml static compilation exception.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library.  If not, see <http://www.gnu.org/licenses/>.
 *)

open Printf

type json = Yojson.Basic.json

type http_error = {
  http_error_code : int;
  http_error_info : string;
}

type wiki_error = {
  wiki_error_code : string;
  wiki_error_info : string;
  wiki_error_details : string option
}

type 'a request = {
  request_method : [`GET | `POST];
  request_params : (string * string) list;
  request_decode : Kojson.jain -> 'a * Kojson.jain;
}

exception Http_error of http_error
exception Wiki_error of wiki_error
