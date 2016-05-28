export function put(key : string, value : any) : void {
  window.localStorage.setItem(key, value);
}

export function get(key : string) : any {
  return window.localStorage.getItem(key);
}

export function remove(key) : void {
  window.localStorage.removeItem(key);
}

export function clear() : void {
  window.localStorage.clear();
}
