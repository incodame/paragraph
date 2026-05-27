import { NxWelcome } from './nx-welcome';
import { Route } from '@angular/router';

export const appRoutes: Route[] = [
  {
    path: 'pavements',
    loadChildren: () => import('pavements/Routes').then((m) => m!.remoteRoutes),
  },
  {
    path: 'queries',
    loadChildren: () => import('queries/Routes').then((m) => m!.remoteRoutes),
  },
  {
    path: '',
    component: NxWelcome,
  },
];
